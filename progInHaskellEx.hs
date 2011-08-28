doubleMe x = x + x

doubleUs x y = x*2 + y*2

pr [] = 1
pr (x:xs) = x * pr xs

su [] = 0
su (x:xs) = x + su xs

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
				where 
				smaller = [a|a<-xs,a < x]
				larger = [b|b<-xs, b>x]

rqsort [] = []
rqsort (x:xs) = rqsort larger ++ [x] ++ qsort smaller
				where 
				smaller = [a|a<-xs,a <= x]
				larger = [b|b<-xs, b>x]