set_option autoImplicit false

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

-- -- also need to define Either since not a native type in lean4
-- inductive Either (α : Type) (β : Type) : Type
--   | Left  : α -> Either α β
--   | Right : β -> Either α β
-- use Except instead of Either

-- %runElab derive "JSONPathElement" [Show,Eq]

-- public export
-- JSONPath : Type
-- JSONPath = List JSONPathElement
abbrev JSONPath : Type := List JSONPathElement

-- public export
-- JSONErr : Type
-- JSONErr = (JSONPath,String)
abbrev JSONErr : Type := Prod JSONPath String

-- public export
-- Result : Type -> Type
-- Result = Either JSONErr
abbrev Result : Type -> Type := Sum JSONErr

-- public export
-- Parser : Type -> Type -> Type
-- Parser v a = v -> Either JSONErr a
abbrev Parser (value  : Type) (α : Type) :=
  value -> Sum JSONErr α

-- public export
-- orElse : Either a b -> Lazy (Either a b) -> Either a b
-- orElse r@(Right _) _ = r
-- orElse _           v = v
def hOrElse
  {α β: Type}
  (e₁ : Sum α β) (f : Thunk (Sum α β)) : Sum α β :=
  match e₁, Thunk.get f with
  | Sum.inr r, _ => Sum.inr r
  | _, v => v

-- public export
-- (<|>) : Parser v a -> Parser v a -> Parser v a
-- f <|> g = \vv => f vv `orElse` g vv
def seq {v a : Type} (f : Parser v a) (g : Parser v a) : Parser v a := λ vv =>
 hOrElse (f vv) (g vv)

infixr:50 " <|> " => seq

-- record Bounds where
--   constructor MkBounds
--   ||| 0-based first line
--   startLine : Int
--   ||| 0-based first col
--   startCol : Int
--   ||| 0-based last line of bound
--   endLine : Int
--   ||| 0-based first column after bound
--   endCol : Int
structure Bounds where
  startLine: Int
  startCol: Int
  endLine: Int
  endCol: Int
deriving Repr

-- record FileContext where
--   constructor MkFileContext
--   file : String
--   range : Bounds
structure FileContext where
  file : String
  range : Bounds
deriving Repr

-- public export
-- data DecodingErr : Type where
--   JErr      : JSONErr -> DecodingErr
--   JParseErr : (FileContext,ParseErr)-> DecodingErr

-- export
-- SExpable FileContext where
--   toSExp fc =
--     SExpList [ SExpList
--                [ SymbolAtom "filename", toSExp fc.file ]
--              , SExpList [ SymbolAtom "start"
--                         , IntegerAtom (cast fc.range.startLine)
--                         , IntegerAtom (cast fc.range.startCol)
--                         ]
--              , SExpList [ SymbolAtom "end"
--                         , IntegerAtom (cast fc.range.endLine)
--                         , IntegerAtom (cast fc.range.endCol)
--                         ]
--              ]

-- export
-- FromSExpable FileContext where
--   fromSExp (SExpList [ SExpList
--                [ SymbolAtom "filename", filenameSExp ]
--              , SExpList [ SymbolAtom "start"
--                         , IntegerAtom startLine
--                         , IntegerAtom startCol
--                         ]
--              , SExpList [ SymbolAtom "end"
--                         , IntegerAtom endLine
--                         , IntegerAtom endCol
--                         ]
--              ]) = do file <- fromSExp filenameSExp
--                      pure $ MkFileContext {file, range = MkBounds
--                        { startLine = cast startLine
--                        , startCol  = cast startCol
--                        , endLine   = cast endLine
--                        , endCol    = cast endCol
--                        }}
--   fromSExp _ = Nothing

inductive ParseError : (token err : Type) → Type where
| EOI : ParseError

inductive DecodingErr : Type where
| JErr : JSONErr → DecodingErr
| JParseErr : FileContext × ParseErr → DecodingErr

-- %runElab derive "DecodingErr" [Show,Eq]

-- public export
-- DecodingResult : Type -> Type
-- DecodingResult = Either DecodingErr

class Encoder (value : Type) where
  stringify : value -> String
  string : String -> value
