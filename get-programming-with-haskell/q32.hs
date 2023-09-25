counter x = 
    let x = x + 1
    in
        let x = x + 1
        in
            x


counter3 x = (\x -> x + 1)
              ((\x -> x + 1)
               ((\x -> x) x))


counter4  x = (\x -> x + 1)((\x -> x + 1) ((\x -> x) x))
