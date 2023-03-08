Function BTD, bnum

    tem = byte(bnum) - 48
    texp = 2l^(lindgen(n_elements(tem)))
    return, long(total(texp*reverse(tem)))
end
