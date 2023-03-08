Function Clean_sder, sd, mwid

	on_error, 1

	n = n_elements(sd)
	res = sd
	mwid = Default(mwid,3l,/dtyp)
	kgen = (findgen(2*mwid-1) - (mwid-1.))/2

	iter = 0

	repeat begin
		count = 0
		ext = Extrema(res,sig=sig,num=num,thre=Toler())
		if num gt 0 then begin
			if sig[0] eq (-1) then begin
				ext = [0,ext]
				sig = [1,sig]
				num = num + 1
			endif
			if sig[num-1] eq (-1) then begin
				ext = [ext,n-1]
				sig = [sig,1]
				num = num + 1
			endif
		endif

		if num ge 3 then begin
			imx = num/2
			dum = 2*lindgen(imx)
			fdifh = (res[ext[dum+2]] - res[ext[dum]])/2.
			sdifh = (res[ext[dum+2]] - 2*res[ext[dum+1]] + res[ext[dum]])/2.
			wid = ext[dum+2] - ext[dum] + 1
			rmin = ext[dum+1] - ext[dum]
			for i = 0l, imx - 1 do begin
				if wid[i] lt mwid then begin
					k = kgen[mwid - wid[i] + 2*lindgen(wid[i])]
					h2 = 1 - 12./(wid[i]^2-1)*k^2
					h3 = k*(1 - 20./(3*wid[i]^2-7)*k^2)
					a3 = fdifh[i]/h3[0]
					a2 = (a3*h3[rmin[i]] - sdifh[i])/(h2[0] - h2[rmin[i]])

					print, i, a2, a3
					print, k
					print, h3
					print
					res[ext[2*i]:ext[2*i+2]] = $
					res[ext[2*i]:ext[2*i+2]] + a2*h2 + a3*h3
					count = count + 1
					i = i + 1
				endif
			endfor
		endif
		iter = iter + 1
		print
		print, num, count
		print, iter
		stop

	endrep until count eq 0

	return, res
end