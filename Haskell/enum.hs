data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat

isWeekend :: Day -> Bool
isWeekend d
 = case d of
	Sat -> True
	Sun -> True
	_ -> False
