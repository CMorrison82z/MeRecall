-- Summary : Boolean Algebra of Sets, evaluated using binary encoding (bitflags)
--
--
-- Description : All boolean functions operate on a set (whatever appears as single element operation is actually a set with 1 element).
--
-- Language Syntax :
--      `!` : "not". Note to take effect on a set of more than one element, must wrap the operand in paranthesis.
--      `*` or `abc...` : "and" or "intersection" or "product". AND is implicit, so can be omitted
--      `+` : "or" or "union" or "sum"
--      Example : ab == a * b -> "a and b"
--      Example : !ab -> "Not 'a' and 'b'"
--      Example : !(ab) -> "Not 'a and b'"
--      Example : !a + b -> "Not 'a', or 'b'"
--      Example : !(a + b) -> "Not 'a or b'"
--      Example : !(a b c) + x y z -> "Not 'a, b, and c', or 'x, y, and z'"
data QLang = Func QFunc | Tag QTag

data QFunc = QAnd | QOr | QNot

-- NOTE: `Text` instead of `String` ?
data QTag = NoTag | AllTag | SomeTag String

newtype QStack = QStack ([QTag], [QFunc])

evalStack qstack candTags = go qstack False
    where
        go QStack (a:b:ts, (QFunc QAnd):fs) r = hasAllS [a, b] candTags
        go QStack (a:b:ts, (QFunc QOr):fs) r = hasAnyS [a, b] candTags
        go QStack (a:ts, (QFunc QNot):fs) r =
