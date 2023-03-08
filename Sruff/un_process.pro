Function UN_process, code, earr, parr, filts, filth, mirrs, mirans

    if code eq 3 or code eq 15 then begin
	for i = 0, n_elements(filts) - 1 do parr = $
	    parr*exp(-0.1*filth(i)*Abs_coeff(earr,elem = filts(i)))
    endif

    if code eq 12 or code eq 15 then begin
	for i = 0, n_elements(mirrs) - 1 do parr = $
	    parr*Mirror(earr,1e-3*mirans(i),elem = mirrs(i))
    endif

    return, parr
end
