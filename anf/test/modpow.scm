(letrec ((_modulo-power14 
                  (lambda (_base15 _exp16 _n17) 
                    (let ((_p52 (= _exp16 0))) 
                      (if _p52 1 
                          (let ((_p53 (odd? _exp16))) 
                            (if _p53 
                                (let ((_p54 (- _exp16 1))) 
                                  (let ((_p55 (_modulo-power14 _base15 _p54 _n17))) 
                                    (let ((_p56 (* _base15 _p55))) (modulo _p56 _n17)))) 
                                (let ((_p57 (/ _exp16 2))) 
                                  (let ((_p58 (_modulo-power14 _base15 _p57 _n17))) 
                                    (let ((_p59 _p58)) 
                                      (modulo _p59 _n17)))))))))))
  (_modulo-power14 11 133 177))