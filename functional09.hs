--rewrite list reverse using left fold
reverse_left l = foldl (flip (:)) [] l 

--rewrite list reverse using right fold
reverse_right l = foldr (\x acc -> acc ++ [x]) [] l

--which version is more efficient, and why?
    --foldl สามารถทำงานได้อย่างมีประสิทธิภาพมากกว่าเนื่องจาก เป็นการทำงานโดยมี O(n) แต่ foldr เป็นการทำงานที่มี O(n^2)

--rewrite map using fold
map_fold f l = foldr ((:).(f)) [] l

--rewrite filter using fold
filter_fold f l = foldr (\x acc-> if f(x) then x:acc else acc) [] l
