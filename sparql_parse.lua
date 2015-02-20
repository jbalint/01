local lpeg = require('lpeg')
local re = require('re')

-- lpeg.Cc(values) - the given values (matches the empty string)
-- lpeg.Ct(patt)   - a table with all captures from patt
local P, R, S, C, Cc, Ct = lpeg.P, lpeg.R, lpeg.S, lpeg.C, lpeg.Cc, lpeg.Ct

Variable = P"?" * C(R("az", "AZ") * P(R("az", "AZ", "09") + P"_")^0)

Iri = ""






-- [67]  IRIref  ::=  IRI_REF |PrefixedName
-- IRIRef = IRI_REF + PrefixedName
-- -- [68]  PrefixedName  ::=  PNAME_LN | PNAME_NS
-- PrefixedName = PNAME_LN + PNAME_NS
-- -- [69]  BlankNode  ::=  BLANK_NODE_LABEL |ANON
-- BlankNode = BLANK_NODE_LABEL + ANON

-- [95]  PN_CHARS_BASE  ::=  [A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
PN_CHARS_BASE = R"AZ" + R"az" + R"\xc0\xd6" + R"\xd8\xf6" -- TODO rest of Unicode skipped
-- [91]  ECHAR  ::=  '\' [tbnrf\"']
ECHAR = P"\\" * S"tbnrf\"'"
-- [86]  EXPONENT  ::=  [eE] [+-]? [0-9]+
EXPONENT = S"eE" * S"+-"^-1 * R"09"^1
-- [93]  WS  ::=  #x20 | #x9 | #xD | #xA
WS = S"\x20\x09\x0d\x0a"

-- [96]  PN_CHARS_U  ::=  PN_CHARS_BASE | '_'
PN_CHARS_U = PN_CHARS_BASE + P"_"
-- [87]  STRING_LITERAL1  ::=  "'" ( ([^#x27#x5C#xA#xD]) | ECHAR )* "'"
STRING_LITERAL1 = "'" * (re.compile("[^\x27\x5C\x0A\x0D]") + ECHAR)^0 * "'"
-- [88]  STRING_LITERAL2  ::=  '"' ( ([^#x22#x5C#xA#xD]) | ECHAR )* '"'
STRING_LITERAL2 = "'" * (re.compile("[^\x22\x5C\x0A\x0D]") + ECHAR)^0 * "'"
-- [89]  STRING_LITERAL_LONG1  ::=  "'''" ( ( "'" | "''" )? ( [^'\] | ECHAR ) )* "'''"
STRING_LITERAL_LONG1 = ""
-- [90]  STRING_LITERAL_LONG2  ::=  '"""' ( ( '"' | '""' )? ( [^"\] | ECHAR ) )* '"""'
STRING_LITERAL_LONG2 = ""

-- [98]  PN_CHARS  ::=  PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
PN_CHARS = PN_CHARS_U + P"-" + R"09" + P"\xB7" -- TODO rest skipped. Unicode in Lua??

-- [100]  PN_LOCAL  ::=  ( PN_CHARS_U | [0-9] ) ((PN_CHARS|'.')* PN_CHARS)?
-- Note that SPARQL local names allow leading digits while XML local names do not.

-- [99]  PN_PREFIX  ::=  PN_CHARS_BASE ((PN_CHARS|'.')* PN_CHARS)?
PN_PREFIX = PN_CHARS_BASE * ((PN_CHARS + S".")^0 * PN_CHARS)^-1


-- [70]  IRI_REF  ::=  '<' ([^<>"{}|^`\]-[#x00-#x20])* '>'
IRI_REF = P"<" * C(P(re.compile("[^<>\"{}|^`\\]"))^0) * P">"
-- [71]  PNAME_NS  ::=  PN_PREFIX? ':'
PNAME_NS = PN_PREFIX^-1 * P":"
-- [72]  PNAME_LN  ::=  PNAME_NS PN_LOCAL

-- [73]  BLANK_NODE_LABEL  ::=  '_:' PN_LOCAL
-- [74]  VAR1  ::=  '?' VARNAME
-- [75]  VAR2  ::=  '$' VARNAME
-- [76]  LANGTAG  ::=  '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
-- [77]  INTEGER  ::=  [0-9]+
-- [78]  DECIMAL  ::=  [0-9]+ '.' [0-9]* | '.' [0-9]+
-- [79]  DOUBLE  ::=  [0-9]+ '.' [0-9]* EXPONENT | '.' ([0-9])+ EXPONENT | ([0-9])+ EXPONENT
-- [80]  INTEGER_POSITIVE  ::=  '+' INTEGER
-- [81]  DECIMAL_POSITIVE  ::=  '+' DECIMAL
-- [82]  DOUBLE_POSITIVE  ::=  '+' DOUBLE
-- [83]  INTEGER_NEGATIVE  ::=  '-' INTEGER
-- [84]  DECIMAL_NEGATIVE  ::=  '-' DECIMAL
-- [85]  DOUBLE_NEGATIVE  ::=  '-' DOUBLE
-- [92]  NIL  ::=  '(' WS* ')'
-- [94]  ANON  ::=  '[' WS* ']'
-- [97]  VARNAME  ::=  ( PN_CHARS_U | [0-9] ) ( PN_CHARS_U | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040] )*

-- [11]  NamedGraphClause  ::=  'NAMED' SourceSelector
-- [10]  DefaultGraphClause  ::=  SourceSelector
-- [9]  DatasetClause  ::=  'FROM' ( DefaultGraphClause | NamedGraphClause )
-- [8]  AskQuery  ::=  'ASK' DatasetClause* WhereClause
-- [7]  DescribeQuery  ::=  'DESCRIBE' ( VarOrIRIref+ | '*' ) DatasetClause* WhereClause? SolutionModifier
-- [6]  ConstructQuery  ::=  'CONSTRUCT' ConstructTemplate DatasetClause* WhereClause SolutionModifier
-- [5]  SelectQuery  ::=  'SELECT' ( 'DISTINCT' | 'REDUCED' )? ( Var+ | '*' ) DatasetClause* WhereClause SolutionModifier
-- [4]  PrefixDecl  ::=  'PREFIX' PNAME_NS IRI_REF
PrefixDecl = P"PREFIX" * PNAME_NS * IRI_REF
-- [3]  BaseDecl  ::=  'BASE' IRI_REF
BaseDecl = P"BASE" * IRI_REF
-- [2]  Prologue  ::=  BaseDecl? PrefixDecl*
Prologue = BaseDecl^-1 * PrefixDecl^0
-- [1]  Query  ::=  Prologue
-- ( SelectQuery | ConstructQuery | DescribeQuery | AskQuery )
Query = Prologue -- * (SelectQuery) -- + ConstructQuery + DescribeQuery + AskQuery)

-- [12]  SourceSelector  ::=  IRIref
-- [13]  WhereClause  ::=  'WHERE'? GroupGraphPattern
-- [14]  SolutionModifier  ::=  OrderClause? LimitOffsetClauses?
-- [15]  LimitOffsetClauses  ::=  ( LimitClause OffsetClause? | OffsetClause LimitClause? )
-- [16]  OrderClause  ::=  'ORDER' 'BY' OrderCondition+
--    [17]  OrderCondition  ::=   ( ( 'ASC' | 'DESC' ) BrackettedExpression )
-- | ( Constraint | Var )
-- [18]  LimitClause  ::=  'LIMIT' INTEGER
-- [19]  OffsetClause  ::=  'OFFSET' INTEGER
-- [20]  GroupGraphPattern  ::=  '{' TriplesBlock? ( ( GraphPatternNotTriples | Filter ) '.'? TriplesBlock? )* '}'
-- [21]  TriplesBlock  ::=  TriplesSameSubject ( '.' TriplesBlock? )?
-- [22]  GraphPatternNotTriples  ::=  OptionalGraphPattern | GroupOrUnionGraphPattern | GraphGraphPattern
-- [23]  OptionalGraphPattern  ::=  'OPTIONAL' GroupGraphPattern
-- [24]  GraphGraphPattern  ::=  'GRAPH' VarOrIRIref GroupGraphPattern
-- [25]  GroupOrUnionGraphPattern  ::=  GroupGraphPattern ( 'UNION' GroupGraphPattern )*
--    [26]  Filter  ::=  'FILTER' Constraint
-- [27]  Constraint  ::=  BrackettedExpression | BuiltInCall | FunctionCall
-- [28]  FunctionCall  ::=  IRIref ArgList
-- [29]  ArgList  ::=  ( NIL | '(' Expression ( ',' Expression )* ')' )
-- [30]  ConstructTemplate  ::=  '{' ConstructTriples? '}'
-- [31]  ConstructTriples  ::=  TriplesSameSubject ( '.' ConstructTriples? )?
-- [32]  TriplesSameSubject  ::=  VarOrTerm PropertyListNotEmpty |TriplesNode PropertyList
-- [33]  PropertyListNotEmpty  ::=  Verb ObjectList ( ';' ( Verb ObjectList )? )*
--    [34]  PropertyList  ::=  PropertyListNotEmpty?
-- [35]  ObjectList  ::=  Object ( ',' Object )*
--    [36]  Object  ::=  GraphNode
-- [37]  Verb  ::=  VarOrIRIref | 'a'
-- [38]  TriplesNode  ::=  Collection |BlankNodePropertyList
-- [39]  BlankNodePropertyList  ::=  '[' PropertyListNotEmpty ']'
-- [40]  Collection  ::=  '(' GraphNode+ ')'
-- [41]  GraphNode  ::=  VarOrTerm |TriplesNode
-- [42]  VarOrTerm  ::=  Var | GraphTerm
-- [43]  VarOrIRIref  ::=  Var | IRIref
-- [44]  Var  ::=  VAR1 | VAR2
-- [45]  GraphTerm  ::=  IRIref |RDFLiteral |NumericLiteral |BooleanLiteral |BlankNode |NIL
-- [46]  Expression  ::=  ConditionalOrExpression
-- [47]  ConditionalOrExpression  ::=  ConditionalAndExpression ( '||' ConditionalAndExpression )*
--    [48]  ConditionalAndExpression  ::=  ValueLogical ( '&&' ValueLogical )*
--    [49]  ValueLogical  ::=  RelationalExpression
-- [50]  RelationalExpression  ::=  NumericExpression ( '=' NumericExpression | '!=' NumericExpression | '<' NumericExpression | '>' NumericExpression | '<=' NumericExpression | '>=' NumericExpression )?
-- [51]  NumericExpression  ::=  AdditiveExpression
-- [52]  AdditiveExpression  ::=  MultiplicativeExpression ( '+' MultiplicativeExpression | '-' MultiplicativeExpression | NumericLiteralPositive | NumericLiteralNegative )*
--    [53]  MultiplicativeExpression  ::=  UnaryExpression ( '*' UnaryExpression | '/' UnaryExpression )*
--    [54]  UnaryExpression  ::=    '!' PrimaryExpression 
-- |'+' PrimaryExpression 
-- |'-' PrimaryExpression 
-- |PrimaryExpression
-- [55]  PrimaryExpression  ::=  BrackettedExpression | BuiltInCall | IRIrefOrFunction | RDFLiteral | NumericLiteral | BooleanLiteral | Var
-- [56]  BrackettedExpression  ::=  '(' Expression ')'
-- [57]  BuiltInCall  ::=    'STR' '(' Expression ')' 
-- |'LANG' '(' Expression ')' 
-- |'LANGMATCHES' '(' Expression ',' Expression ')' 
-- |'DATATYPE' '(' Expression ')' 
-- |'BOUND' '(' Var ')' 
-- |'sameTerm' '(' Expression ',' Expression ')' 
-- |'isIRI' '(' Expression ')' 
-- |'isURI' '(' Expression ')' 
-- |'isBLANK' '(' Expression ')' 
-- |'isLITERAL' '(' Expression ')' 
-- |RegexExpression
-- [58]  RegexExpression  ::=  'REGEX' '(' Expression ',' Expression ( ',' Expression )? ')'
-- [59]  IRIrefOrFunction  ::=  IRIref ArgList?
-- [60]  RDFLiteral  ::=  String ( LANGTAG | ( '^^' IRIref ) )?
-- [61]  NumericLiteral  ::=  NumericLiteralUnsigned | NumericLiteralPositive | NumericLiteralNegative
-- [62]  NumericLiteralUnsigned  ::=  INTEGER |DECIMAL |DOUBLE
-- [63]  NumericLiteralPositive  ::=  INTEGER_POSITIVE |DECIMAL_POSITIVE |DOUBLE_POSITIVE
-- [64]  NumericLiteralNegative  ::=  INTEGER_NEGATIVE |DECIMAL_NEGATIVE |DOUBLE_NEGATIVE
-- [65]  BooleanLiteral  ::=  'true' |'false'
-- [66]  String  ::=  STRING_LITERAL1 | STRING_LITERAL2 | STRING_LITERAL_LONG1 | STRING_LITERAL_LONG2
-- [67]  IRIref  ::=  IRI_REF |PrefixedName
-- [68]  PrefixedName  ::=  PNAME_LN | PNAME_NS
-- [69]  BlankNode  ::=  BLANK_NODE_LABEL |ANON

print(Query:match("PREFIX :a <abc"))
