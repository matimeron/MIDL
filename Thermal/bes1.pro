Function bes1, q


    nrt = n_elements(q)
    n = 1 + lindgen(n_elements(q))
    res = sqrt(2/!pi/q)*(-1)^(n-1)*(1 + 1./(156*n^2))

    res(0:nrt<30) = beselj(q(0:nrt<30),1)

    return, res
end
