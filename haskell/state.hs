


type State s a = s -> (s,a)

ret :: a -> State s a
ret x = \s -> (s,x)

bind :: State s a -> (a -> State s b) -> State s b
bind m f = \s -> let (s0, a) = m s
                 in f a s0

get :: State s s
get = \s -> (s,s)

put :: s -> State s ()
put v = \s -> (v,())




