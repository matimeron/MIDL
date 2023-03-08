Function Pico_get, dt, horizontal = hor, vertical = ver, raw = raw, $
	mean = mea, sigma = sig, fft = fft

	on_error, 1

	whi = One_of(ver,hor) > 0
	raw = Rascii()

	coo = reform((raw[whi,*] - raw[whi+1,*])/(raw[whi,*] + raw[whi+1,*]))
	len = n_elements(coo)
	tim = dt*findgen(len)
	res = transpose([[tim],[coo]])
	mea = total(coo)/len
	sig = sqrt(total((coo-mea)^2)/(len-1))

	if arg_present(fft) then begin
		fft = Centft(res,/zer,/pos,/squ,/pack)
		fft[0,*] = fft[0,*]/(2*!pi)
		fft[1,*] = sqrt(fft[1,*])
	endif

	return, res
end