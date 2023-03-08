Function Peak_scan, x, y, width = wid, min_width = minw, max_width = maxw, $
	threshold = tre, asym = asm, number = nval, second_der = sder

	on_error, 1
	sinf = machar(double = Isnum(x,/doub) or Isnum(y,/doub))
	res = transpose([sinf.xmax,sinf.xmax])
	nval = 0

	n = n_elements(x)
	wwid = Default(wid,3,/dtyp) > 3
	m = wwid/2
	wwid = 2*m + 1
	if n ge (wwid + 2) then begin
		case n_elements(y) of
			0	:	begin
						wx = 0*x + findgen(n)
						wy = Cast(x,4)
					end
			n	:	begin
						wx = Cast(x,4)
						wy = Cast(y,4)
					end
			else:	message, 'Incompatible array lenghts!'
		endcase
		dx = 1.*(max(wx,min=min) - min)/(n-1)
		wmax = dx*Default(maxw,(n-1)/sqrt(3))
		wmin = dx*Default(minw,wwid/sqrt(3)) < wmax
		avy = Total(Abs_mm(wy))/n
		tre = Default(tre,sqrt(sinf.eps)*avy*wmin) > 0
		asm = Default(asm,1/(2 + exp(1.5)))
	endif else message, 'Insufficient length for peak determination!'

	sder = Smooth_mm(wy,wwid,der=2,/edge)
	sder[0:m-1] = sder[m]
	sder[n-m:n-1] = sder[n-m-1]
	ext = Extrema(sder,signature=sig,num=num,thresh=tre/(wmax))

	if num ge 3 then begin
		if ext[0] gt 0 then fir = 0 else fir = 1
		if ext[num-1] lt n-1 then las = num-1 else las = num-2
		sig = sig[fir:las]
		ext = ext[fir:las]
		num = n_elements(sig)
		if num ge 3 then begin
			if sig[0] eq 1 then fir = 0 else fir = 1
			if sig[num-1] eq 1 then las = num-1 else las = num-2
			sig = sig[fir:las]
			ext = ext[fir:las]
			num = n_elements(sig)
			if num ge 3 then begin
				imx = num/2
				wres = make_array(imx,2,type=Type(wx))
				valid = lonarr(imx)
				for i = 0, imx-1 do begin
					ind = ext[[2*i + [0,1,2]]]
					wres[i,*] = ind[[0,2]] + [-1,1]
					twx = wx[ind]
					tsd = sder[ind]
					twid = (twx[2] - twx[0])/sqrt(3)
					propmin = tsd[0] gt 0 and tsd[1] lt 0 and tsd[2] gt 0
					if twid ge wmin and twid le wmax and propmin then begin
						fdif = tsd[2] - tsd[0]
						sdif = tsd[0] - 2*tsd[1] + tsd[2]
						ivl = (twid/(2*dx))^3*sdif*dx
						if ivl ge tre and abs(fdif/sdif) lt asm then valid[i]=1
					endif
				endfor
				dum = where(valid eq 1, nval)
				if nval gt 0 then res = wres[dum,*]
			endif
		endif
	endif

	return, res
end
