data ListInt = Nil| Cons Int ListInt

length:: ListInt-> Int

length = \(ls :: List a) -> case ls of
	Nil 0
	Cons first rest 1
