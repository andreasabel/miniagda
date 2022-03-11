-- 2012-02-22

cofun T : -(i : Size)|i| -> Set
{ T i = [j < i] -> T j
}

fun bla : [i : Size] |i| -> T i
{ bla i = bla
}
-- should succeed
