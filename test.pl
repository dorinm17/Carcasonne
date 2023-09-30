multiplu(km, m).
multiplu(hm, m).
submultiplu(cm, m).
submultiplu(mm, m).

multiplu(X, Y) :- submultiplu(X, Z), multiplu(Z, Y).