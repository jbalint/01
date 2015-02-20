-- unification/reduction

-- sexp parser

local __dump = require("pl.pretty").dump

lpeg = require("lpeg")
re = require("re")
local P, R, S, V = lpeg.P, lpeg.R, lpeg.S, lpeg.V
local C, Cc, Cf, Cg, Cs, Ct, Cmt = lpeg.C, lpeg.Cc, lpeg.Cf, lpeg.Cg, lpeg.Cs, lpeg.Ct, lpeg.Cmt

-- sections stolen from from turtle parser

local _integer = (P"-"+P"+")^-1*R"09"^1
local integer = Cmt(_integer, function (s, p, v)
					   return p, {t="integer", value=v}
end)

local ws = P"\x09"+P"\x0a"+P"\x0d"+P"\x20"

-- name/atom - starts with lowercase letter
local _name = R"az"*(R"az"+R"AZ"+R"09"+P"_")^0
local name = Cmt(_name, function (s, p, v)
					return p, {t="name", name=v}
end)

-- variable - starts with uppercase letter
local _var = R"AZ"*(R"az"+R"AZ"+R"09"+P"_")^0
local var = Cmt(_var, function (s, p, v)
				   return p, {t="var", var=v}
end)

local term = name+var+integer

local sexp = P{"sexp";
			   _sexp = P"("*ws^0*name*ws^0*Ct(((V"sexp"+term)*ws^0)^0)*P")",
			   sexp = Cmt(Ct(V"_sexp"), function (s, p, v)
							 return p, {t="sexp", name=v[1], args=v[2]}
end)}

--__dump(lpeg.match(sexp, "(a b c 1 2 (d 3) (e 4 (f 5)) 8)"))
