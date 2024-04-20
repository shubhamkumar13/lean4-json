def hello := "world"

-- interface Encoder v where
--   ||| Converts the intermediary data representation
--   ||| to a JSON string.
--   stringify : v -> String

--   ||| Encodes a `String` value.
--   string : String -> v

--   ||| Encodes a `Double` as a JSON `Double`.
--   double : Double -> v

--   ||| Encodes a `Double` as a JSON `Integer`.
--   integer : Integer -> v

--   ||| Encodes a `Bool` as a JSON `Boolean`.
--   boolean : Bool -> v

--   ||| Encodes a `List` of values as a JSON `Array`.
--   array : List v -> v

--   ||| Encodes a `List` key-value pairs as a JSON `Object`
--   object : List (String,v) -> v

--   null : v

-- --------------------------------------------------------------------------------
-- --          Types
-- --------------------------------------------------------------------------------

-- public export
-- data JSONPathElement = Key String | Index Bits32

inductive JSONPathElement where
  | Key : String -> JSONPathElement
  | Index : UInt32 -> JSONPathElement
deriving Repr, DecidableEq

-- also need to define Either since not a native type in lean4
inductive Either (α : Type) (β : Type) : Type
  | Left  : α -> Either α β
  | Right : β -> Either α β

-- %runElab derive "JSONPathElement" [Show,Eq]

-- public export
-- JSONPath : Type
-- JSONPath = List JSONPathElement
def JSONPath : Type := List JSONPathElement

-- public export
-- JSONErr : Type
-- JSONErr = (JSONPath,String)
def JSONErr : Type := Prod JSONPath String

-- public export
-- Result : Type -> Type
-- Result = Either JSONErr
def Result : Type -> Type := Either JSONErr

-- public export
-- Parser : Type -> Type -> Type
-- Parser v a = v -> Either JSONErr a
def Parser (value  : Type) (α : Type) :=
  value -> Either JSONErr α

-- public export
-- orElse : Either a b -> Lazy (Either a b) -> Either a b
-- orElse r@(Right _) _ = r
-- orElse _           v = v
def hOrElse
  {α β : Type}
  {a : Either α β}
  {b : Thunk (Either α β)}
  {c : Either α β}
  (e₁ : a) (f : b) : c :=
  match e₁, f with
  | Either.Right r, _ => Either.Right r
  | _, v => v

-- public export
-- (<|>) : Parser v a -> Parser v a -> Parser v a
-- f <|> g = \vv => f vv `orElse` g vv
def seq (f : Parser value α) (g : Parser value β) : Parser value α := λ v =>
  OrElse (f v) (g v)

-- public export
-- data DecodingErr : Type where
--   JErr      : JSONErr -> DecodingErr
--   JParseErr : (FileContext,ParseErr)-> DecodingErr

-- %runElab derive "DecodingErr" [Show,Eq]

-- public export
-- DecodingResult : Type -> Type
-- DecodingResult = Either DecodingErr

class Encoder (value : Type) where
  stringify : value -> String
  string : String -> value
