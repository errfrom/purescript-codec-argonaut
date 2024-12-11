module Data.Codec.Argonaut.Sum
  ( Encoding(..)
  , FlatEncoding
  , class GCases
  , class GFields
  , class GFlatCases
  , defaultEncoding
  , defaultFlatEncoding
  , enumSum
  , gCasesDecode
  , gCasesEncode
  , gFieldsDecode
  , gFieldsEncode
  , gFlatCasesDecode
  , gFlatCasesEncode
  , sum
  , sumFlat
  , sumFlatWith
  , sumWith
  , taggedSum
  )
  where

import Prelude

import Aeson (Aeson)
import Aeson (fromString) as Aeson
import Control.Alt ((<|>))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core (Json, fromString) as J
import Data.Array (catMaybes)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec (codec', encode)
import Data.Codec as Codec
import Data.Codec.Argonaut (JPropCodec, JsonCodec, JsonDecodeError(..), jobject)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), Sum(..), from, to)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor (dimap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object (Object)
import Foreign.Object as FO
import Foreign.Object as Obj
import Foreign.Object.ST as FOST
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | A helper for defining JSON codecs for "enum" sum types, where every
-- | constructor is nullary, and the type will be encoded as a string.
enumSum
  ∷ ∀ a
  . (a → String)
  → (String → Maybe a)
  → CA.JsonCodec a
enumSum printTag parseTag =
  Codec.codec
    (\j → maybe (Left (CA.UnexpectedValue j)) Right <<< parseTag =<< Codec.decode CA.string j)
    (Codec.encode CA.string <<< printTag)

-- | A helper for defining JSON codecs for sum types. To ensure exhaustivity
-- | there needs to be a mapping to and from a tag type for the type to be
-- | encoded.
-- |
-- | - The first argument is the name of the type being decoded, for error
-- |   message purposes.
-- | - The second argument maps a tag value to a string to use in the encoding.
-- | - The third argument maps a string back to a tag value during decoding.
-- | - The fourth argument returns either a constant value or a decoder function
-- |   based on a tag value.
-- | - The fifth argument returns a tag value and optional encoded value to
-- |   store for a constructor of the sum.
taggedSum
  ∷ ∀ tag a
  . String
  → (tag → String)
  → (String → Maybe tag)
  → (tag → Either a (Aeson → Either CA.JsonDecodeError a))
  → (a → Tuple tag (Maybe Aeson))
  → CA.JsonCodec a
taggedSum name printTag parseTag f g = Codec.codec decodeCase encodeCase
  where
  decodeCase ∷ Aeson → Either CA.JsonDecodeError a
  decodeCase j = lmap (CA.Named name) do
    obj ← Codec.decode CA.jobject j
    tag ← Codec.decode (CA.prop "tag" CA.string) obj
    case parseTag tag of
      Nothing → Left (CA.AtKey "tag" (CA.UnexpectedValue (Aeson.fromString tag)))
      Just t →
        case f t of
          Left a → pure a
          Right decoder → do
            value ← Codec.decode (CA.prop "value" CA.json) obj
            lmap (CA.AtKey "value") (decoder value)

  encodeCase ∷ a → Aeson
  encodeCase a = case g a of
    Tuple tag value →
      Codec.encode CA.jobject $
        FO.runST do
          obj ← FOST.new
          _ ← FOST.poke "tag" (Codec.encode CA.string (printTag tag)) obj
          maybe (pure obj) (\v → FOST.poke "value" v obj) value

--------------------------------------------------------------------------------

data Encoding
  = EncodeNested
      { unwrapSingleArguments ∷ Boolean }
  | EncodeTagged
      { tagKey ∷ String
      , valuesKey ∷ String
      , omitEmptyArguments ∷ Boolean
      , unwrapSingleArguments ∷ Boolean
      }

defaultEncoding ∷ Encoding
defaultEncoding = EncodeTagged
  { tagKey: "tag"
  , valuesKey: "values"
  , unwrapSingleArguments: false
  , omitEmptyArguments: false
  }

--------------------------------------------------------------------------------

sum ∷ ∀ r rep a. Generic a rep ⇒ GCases r rep ⇒ String → Record r → JsonCodec a
sum = sumWith defaultEncoding

sumWith ∷ ∀ r rep a. GCases r rep ⇒ Generic a rep ⇒ Encoding → String → Record r → JsonCodec a
sumWith encoding name r =
  dimap from to $ codec' decode encode
  where
  decode = gCasesDecode encoding r >>> (lmap $ Named name)
  encode = gCasesEncode encoding r

--------------------------------------------------------------------------------

class GCases ∷ Row Type → Type → Constraint
class
  GCases r rep
  where
  gCasesEncode ∷ Encoding → Record r → rep → Aeson
  gCasesDecode ∷ Encoding → Record r → Aeson → Either JsonDecodeError rep

instance gCasesConstructorNoArgs ∷
  ( Row.Cons name Unit () r
  , IsSymbol name
  ) ⇒
  GCases r (Constructor name NoArguments) where
  gCasesEncode ∷ Encoding → Record r → Constructor name NoArguments → Aeson
  gCasesEncode encoding _ _ =
    let
      name = reflectSymbol (Proxy :: _ name) ∷ String
    in
      encodeSumCase encoding name []

  gCasesDecode ∷ Encoding → Record r → Aeson → Either JsonDecodeError (Constructor name NoArguments)
  gCasesDecode encoding _ json = do
    let name = reflectSymbol (Proxy :: _ name) ∷ String

    parseNoFields encoding json name
    pure $ Constructor NoArguments

else instance gCasesConstructorSingleArg ∷
  ( Row.Cons name (JsonCodec a) () r
  , IsSymbol name
  ) ⇒
  GCases r (Constructor name (Argument a)) where
  gCasesEncode ∷ Encoding → Record r → Constructor name (Argument a) → Aeson
  gCasesEncode encoding r (Constructor (Argument x)) =
    let
      codec = Record.get (Proxy :: _ name) r ∷ JsonCodec a
      name = reflectSymbol (Proxy :: _ name) :: String
    in
      encodeSumCase encoding name [ CA.encode codec x ]

  gCasesDecode ∷ Encoding → Record r → Aeson → Either JsonDecodeError (Constructor name (Argument a))
  gCasesDecode encoding r json = do
    let name = reflectSymbol (Proxy :: _ name) :: String

    field ← parseSingleField encoding json name ∷ _ Aeson
    let codec = Record.get (Proxy :: _ name) r ∷ JsonCodec a
    result ← CA.decode codec field ∷ _ a
    pure $ Constructor (Argument result)

else instance gCasesConstructorManyArgs ∷
  ( Row.Cons name codecs () r
  , GFields codecs args
  , IsSymbol name
  ) ⇒
  GCases r (Constructor name args) where
  gCasesEncode ∷ Encoding → Record r → Constructor name args → Aeson
  gCasesEncode encoding r (Constructor rep) =
    let
      codecs = Record.get (Proxy :: _ name) r ∷ codecs
      name = reflectSymbol (Proxy :: _ name) ∷ String
      jsons = gFieldsEncode encoding codecs rep ∷ Array Aeson
    in
      encodeSumCase encoding name jsons

  gCasesDecode ∷ Encoding → Record r → Aeson → Either JsonDecodeError (Constructor name args)
  gCasesDecode encoding r json = do
    let name = reflectSymbol (Proxy :: _ name) ∷ String

    jsons ← parseManyFields encoding json name ∷ _ (Array Aeson)
    let codecs = Record.get (Proxy :: _ name) r ∷ codecs
    result ← gFieldsDecode encoding codecs jsons ∷ _ args
    pure $ Constructor result

instance gCasesSum ∷
  ( GCases r1 (Constructor name lhs)
  , GCases r2 rhs
  , Row.Cons name codec () r1
  , Row.Cons name codec r2 r
  , Row.Union r1 r2 r
  , Row.Lacks name r2
  , IsSymbol name
  ) ⇒
  GCases r (Sum (Constructor name lhs) rhs) where
  gCasesEncode ∷ Encoding → Record r → Sum (Constructor name lhs) rhs → Aeson
  gCasesEncode encoding r =
    let
      codec = Record.get (Proxy :: _ name) r ∷ codec
      r1 = Record.insert (Proxy :: _ name) codec {} ∷ Record r1
      r2 = unsafeDelete (Proxy :: _ name) r ∷ Record r2
    in
      case _ of
        Inl lhs → gCasesEncode encoding r1 lhs
        Inr rhs → gCasesEncode encoding r2 rhs

  gCasesDecode ∷ Encoding → Record r → Aeson → Either JsonDecodeError (Sum (Constructor name lhs) rhs)
  gCasesDecode encoding r tagged = do
    let
      codec = Record.get (Proxy :: _ name) r ∷ codec
      r1 = Record.insert (Proxy :: _ name) codec {} ∷ Record r1
      r2 = Record.delete (Proxy :: _ name) r ∷ Record r2
    let
      lhs = gCasesDecode encoding r1 tagged ∷ _ (Constructor name lhs)
      rhs = gCasesDecode encoding r2 tagged ∷ _ rhs
    (Inl <$> lhs) <|> (Inr <$> rhs)

--------------------------------------------------------------------------------

class GFields ∷ Type → Type → Constraint
class GFields codecs rep where
  gFieldsEncode ∷ Encoding → codecs → rep → Array Aeson
  gFieldsDecode ∷ Encoding → codecs → Array Aeson → Either JsonDecodeError rep

instance gFieldsArgument ∷ GFields (JsonCodec a) (Argument a) where
  gFieldsEncode ∷ Encoding → JsonCodec a → Argument a → Array Aeson
  gFieldsEncode _ codec (Argument val) = [ CA.encode codec val ]

  gFieldsDecode ∷ Encoding → JsonCodec a → Array Aeson → Either JsonDecodeError (Argument a)
  gFieldsDecode _ codec jsons = do
    json ←
      ( case jsons of
          [ head ] → pure head
          _ → Left $ TypeMismatch "Expecting exactly one element"
      ) ∷ _ Aeson
    res ← CA.decode codec json ∷ _ a
    pure $ Argument res

instance gFieldsProduct ∷
  ( GFields codec rep
  , GFields codecs reps
  ) ⇒
  GFields (codec /\ codecs) (Product rep reps) where
  gFieldsEncode ∷ Encoding → (codec /\ codecs) → Product rep reps → Array Aeson
  gFieldsEncode encoding (codec /\ codecs) (Product rep reps) =
    let
      r1 = gFieldsEncode encoding codec rep ∷ Array Aeson
      r2 = gFieldsEncode encoding codecs reps ∷ Array Aeson
    in
      r1 <> r2

  gFieldsDecode ∷ Encoding → (codec /\ codecs) → Array Aeson → Either JsonDecodeError (Product rep reps)
  gFieldsDecode encoding (codec /\ codecs) jsons = do
    { head, tail } ←
      (Array.uncons jsons # note (TypeMismatch "Expecting at least one element"))
        ∷ _ { head ∷ Aeson, tail ∷ Array Aeson }
    rep ← gFieldsDecode encoding codec [ head ] ∷ _ rep
    reps ← gFieldsDecode encoding codecs tail ∷ _ reps
    pure $ Product rep reps

--------------------------------------------------------------------------------

checkTag ∷ String → Object Aeson → String → Either JsonDecodeError Unit
checkTag tagKey obj expectedTag = do
  val ←
    ( Obj.lookup tagKey obj
        # note (TypeMismatch ("Expecting a tag property `" <> tagKey <> "`"))
    ) ∷ _ Aeson
  tag ← CA.decode CA.string val ∷ _ String
  unless (tag == expectedTag)
    $ Left
    $ TypeMismatch ("Expecting tag `" <> expectedTag <> "`, got `" <> tag <> "`")

parseNoFields ∷ Encoding → Aeson → String → Either JsonDecodeError Unit
parseNoFields encoding json expectedTag =
  case encoding of
    EncodeNested {} → do
      obj ← CA.decode jobject json
      val ←
        ( Obj.lookup expectedTag obj # note (TypeMismatch ("Expecting a property `" <> expectedTag <> "`"))
        ) ∷ _ Aeson
      fields ← CA.decode CA.jarray val ∷ _ (Array Aeson)
      when (fields /= [])
        $ Left
        $ TypeMismatch "Expecting an empty array"

    EncodeTagged { tagKey, valuesKey, omitEmptyArguments } → do
      obj ← CA.decode jobject json
      checkTag tagKey obj expectedTag
      when (not omitEmptyArguments) do
        val ←
          ( Obj.lookup valuesKey obj
              # note (TypeMismatch ("Expecting a value property `" <> valuesKey <> "`"))
          ) ∷ _ Aeson
        fields ← CA.decode CA.jarray val ∷ _ (Array Aeson)
        when (fields /= [])
          $ Left
          $ TypeMismatch "Expecting an empty array"

parseSingleField ∷ Encoding → Aeson → String → Either JsonDecodeError Aeson
parseSingleField encoding json expectedTag = case encoding of
  EncodeNested { unwrapSingleArguments } → do
    obj ← CA.decode jobject json
    val ←
      ( Obj.lookup expectedTag obj # note (TypeMismatch ("Expecting a property `" <> expectedTag <> "`"))
      ) ∷ _ Aeson
    if unwrapSingleArguments then
      pure val
    else do
      fields ← CA.decode CA.jarray val
      case fields of
        [ head ] → pure head
        _ → Left $ TypeMismatch "Expecting exactly one element"

  EncodeTagged { tagKey, valuesKey, unwrapSingleArguments } → do
    obj ← CA.decode jobject json
    checkTag tagKey obj expectedTag
    val ←
      ( Obj.lookup valuesKey obj
          # note (TypeMismatch ("Expecting a value property `" <> valuesKey <> "`"))
      ) ∷ _ Aeson
    if unwrapSingleArguments then
      pure val
    else do
      fields ← CA.decode CA.jarray val
      case fields of
        [ head ] → pure head
        _ → Left $ TypeMismatch "Expecting exactly one element"

parseManyFields ∷ Encoding → Aeson → String → Either JsonDecodeError (Array Aeson)
parseManyFields encoding json expectedTag =
  case encoding of
    EncodeNested {} → do
      obj ← CA.decode jobject json
      val ←
        ( Obj.lookup expectedTag obj # note (TypeMismatch ("Expecting a property `" <> expectedTag <> "`"))
        ) ∷ _ Aeson
      CA.decode CA.jarray val

    EncodeTagged { tagKey, valuesKey } → do
      obj ← CA.decode jobject json
      checkTag tagKey obj expectedTag
      val ←
        ( Obj.lookup valuesKey obj
            # note (TypeMismatch ("Expecting a value property `" <> valuesKey <> "`"))
        ) ∷ _ Aeson
      CA.decode CA.jarray val

encodeSumCase ∷ Encoding → String → Array Aeson → Aeson
encodeSumCase encoding tag jsons =
  case encoding of
    EncodeNested { unwrapSingleArguments } →
      let
        val = case jsons of
          [] → CA.encode CA.jarray []
          [ json ] | unwrapSingleArguments → json
          manyJsons → CA.encode CA.jarray manyJsons
      in
        encode jobject $ Obj.fromFoldable
          [ tag /\ val
          ]

    EncodeTagged { tagKey, valuesKey, unwrapSingleArguments, omitEmptyArguments } →
      let
        tagEntry =
          Just (tagKey /\ CA.encode CA.string tag) ∷ Maybe (String /\ Aeson)
        valEntry =
          case jsons of
            [] | omitEmptyArguments → Nothing
            [ json ] | unwrapSingleArguments → Just (valuesKey /\ json)
            manyJsons → Just (valuesKey /\ CA.encode CA.jarray manyJsons)
      in
        encode jobject $ Obj.fromFoldable $ catMaybes
          [ tagEntry, valEntry ]

type FlatEncoding (tag ∷ Symbol) =
  { tag ∷ Proxy tag
  }

defaultFlatEncoding ∷ FlatEncoding "tag"
defaultFlatEncoding = { tag: Proxy }

sumFlat ∷ ∀ r rep a. GFlatCases "tag" r rep ⇒ Generic a rep ⇒ String → Record r → JsonCodec a
sumFlat = sumFlatWith defaultFlatEncoding

sumFlatWith ∷ ∀ tag r rep a. GFlatCases tag r rep ⇒ Generic a rep ⇒ FlatEncoding tag -> String → Record r → JsonCodec a
sumFlatWith _ name r =
  dimap from to $ codec' dec enc
  where
  dec = gFlatCasesDecode (Proxy :: _ tag) r >>> (lmap $ Named name)
  enc = gFlatCasesEncode (Proxy :: _ tag) r

class GFlatCases ∷ Symbol → Row Type → Type → Constraint
class
  GFlatCases tag r rep
  where
  gFlatCasesEncode ∷ Proxy tag -> Record r → rep → Aeson
  gFlatCasesDecode ∷ Proxy tag ->Record r → Aeson → Either JsonDecodeError rep

instance gFlatCasesConstructorNoArg ∷
  ( Row.Cons name Unit () rc
  , Row.Cons tag String () rf
  , IsSymbol name
  , IsSymbol tag
  ) ⇒
  GFlatCases tag rc (Constructor name NoArguments) where
  gFlatCasesEncode ∷ Proxy tag -> Record rc → Constructor name NoArguments → Aeson
  gFlatCasesEncode _ _ (Constructor NoArguments) =
    let
      name = reflectSymbol (Proxy :: _ name) ∷ String
      propCodec = CAR.record {} ∷ JPropCodec {}
      propCodecWithTag = CA.recordProp (Proxy :: _ tag) CA.string propCodec ∷ JPropCodec (Record rf)
      codecWithTag = CA.object ("case " <> name) propCodecWithTag ∷ JsonCodec (Record rf)
      rcWithTag = Record.insert (Proxy :: _ tag) name {} ∷ Record rf
    in
      CA.encode codecWithTag rcWithTag

  gFlatCasesDecode ∷ Proxy tag -> Record rc → Aeson → Either JsonDecodeError (Constructor name NoArguments)
  gFlatCasesDecode _ _ json = do
    let
      name = reflectSymbol (Proxy :: _ name) ∷ String

      propCodec = CAR.record {} ∷ JPropCodec {}
      propCodecWithTag = CA.recordProp (Proxy :: _ tag) CA.string propCodec ∷ JPropCodec (Record rf)
      codecWithTag = CA.object ("case " <> name) propCodecWithTag ∷ JsonCodec (Record rf)
    r ← CA.decode codecWithTag json ∷ _ (Record rf)
    let actualTag = Record.get (Proxy :: _ tag) r ∷ String

    when (actualTag /= name)
      $ Left
      $ TypeMismatch ("Expecting tag `" <> name <> "`, got `" <> actualTag <> "`")

    pure (Constructor NoArguments)

instance gFlatCasesConstructorSingleArg ∷
  ( Row.Cons name (JPropCodec (Record rf)) () rc
  , Row.Lacks tag rf
  , Row.Cons tag String rf rf'
  , IsSymbol name
  , IsSymbol tag
  ) ⇒
  GFlatCases tag rc (Constructor name (Argument (Record rf))) where
  gFlatCasesEncode ∷ Proxy tag -> Record rc → Constructor name (Argument (Record rf)) → Aeson
  gFlatCasesEncode _ rc (Constructor (Argument rf)) =
    let
      name = reflectSymbol (Proxy :: _ name) ∷ String
      propCodec = Record.get (Proxy :: _ name) rc ∷ JPropCodec (Record rf)
      propCodecWithTag = CA.recordProp (Proxy :: _ tag) CA.string propCodec ∷ JPropCodec (Record rf')
      codecWithTag = CA.object ("case " <> name) propCodecWithTag ∷ JsonCodec (Record rf')
      rcWithTag = Record.insert (Proxy :: _ tag) name rf ∷ Record rf'
    in
      CA.encode codecWithTag rcWithTag

  gFlatCasesDecode ∷ Proxy tag -> Record rc → Aeson → Either JsonDecodeError (Constructor name (Argument (Record rf)))
  gFlatCasesDecode _ rc json = do
    let
      name = reflectSymbol (Proxy :: _ name) ∷ String
      propCodec = Record.get (Proxy :: _ name) rc ∷ JPropCodec (Record rf)
      propCodecWithTag = CA.recordProp (Proxy :: _ tag) CA.string propCodec ∷ JPropCodec (Record rf')
      codecWithTag = CA.object ("case " <> name) propCodecWithTag ∷ JsonCodec (Record rf')
    r ← CA.decode codecWithTag json ∷ _ (Record rf')

    let actualTag = Record.get (Proxy :: _ tag) r ∷ String
    when (actualTag /= name)
      $ Left
      $ TypeMismatch ("Expecting tag `" <> name <> "`, got `" <> actualTag <> "`")

    let r' = Record.delete (Proxy :: _ tag) r ∷ Record rf
    pure (Constructor (Argument r'))

instance gFlatCasesSum ∷
  ( GFlatCases tag r1 (Constructor name lhs)
  , GFlatCases tag r2 rhs
  , Row.Cons name codec () r1
  , Row.Cons name codec r2 r
  , Row.Union r1 r2 r
  , Row.Lacks name r2
  , IsSymbol name
  ) ⇒
  GFlatCases tag r (Sum (Constructor name lhs) rhs) where
  gFlatCasesEncode ∷ Proxy tag -> Record r → Sum (Constructor name lhs) rhs → Aeson
  gFlatCasesEncode _ r =
    let
      codec = Record.get (Proxy :: _ name) r ∷ codec
      r1 = Record.insert (Proxy :: _ name) codec {} ∷ Record r1
      r2 = unsafeDelete (Proxy :: _ name) r ∷ Record r2
    in
      case _ of
        Inl lhs → gFlatCasesEncode (Proxy :: _ tag) r1 lhs
        Inr rhs → gFlatCasesEncode (Proxy :: _ tag) r2 rhs

  gFlatCasesDecode ∷ Proxy tag -> Record r → Aeson → Either JsonDecodeError (Sum (Constructor name lhs) rhs)
  gFlatCasesDecode _ r tagged = do
    let
      codec = Record.get (Proxy :: _ name) r ∷ codec
      r1 = Record.insert (Proxy :: _ name) codec {} ∷ Record r1
      r2 = Record.delete (Proxy :: _ name) r ∷ Record r2
    let
      lhs = gFlatCasesDecode (Proxy :: _ tag) r1 tagged ∷ _ (Constructor name lhs)
      rhs = gFlatCasesDecode (Proxy :: _ tag) r2 tagged ∷ _ rhs
    (Inl <$> lhs) <|> (Inr <$> rhs)

-- | Same as `Record.delete` but deleting only happens at the type level
-- | and the value is left untouched.
unsafeDelete ∷ ∀ r1 r2 l a. IsSymbol l ⇒ Row.Lacks l r1 ⇒ Row.Cons l a r1 r2 ⇒ Proxy l → Record r2 → Record r1
unsafeDelete _ r = unsafeCoerce r

