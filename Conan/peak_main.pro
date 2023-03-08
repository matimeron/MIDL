Function Peak_main, x, y, width = wid, search = ser, status= stat

	on_error, 1
	sinf = machar(double = Isnum(x,/doub) or Isnum(y,/doub))
	res = sinf.xmax
	stat = 0

	n = n_elements(x)
	wwid = Default(wid,3,/dtyp) > 3
	m = wwid/2
	wwid = 2*m + 1
	if n ge wwid + 2 then begin
		case n_elements(y) of
			0	:	begin
						wx = 0*x + findgen(n)
						wy = x
					end
			n	:	begin
						wx = x
						wy = y
					end
			else:	message, 'Incompatible array lenghts!'
		endcase
	endif

	sder = Smooth_mm(wy,wwid,der=2,/edge)
	sder[0:m-1] = sder[m]
	sder[n-m:n-1] = sder[n-m-1]
	ext = Extrema(sder,/min,num=num)
	gofl = num gt 0
	serfl = 0

	while gofl do begin
		if serfl then begin
			slop = Splin_eval(wx[loc],spcp)
			if slop lt 0 then begin
				sext = Extrema(wy[0:loc-1],/max,num=num)
				if num gt 0 then loc = sext[num-1] else loc = 0
			endif else begin
				sext = Extrema(wy[loc+1:n-1],/max,num=num)
				if num gt 0 then loc = sext[0] else loc = n-1
			endelse
		endif else begin
			dum = min(sder[ext],minext)
			loc = ext[minext]
		endelse
		if loc gt 0 and loc lt (n-1) then begin
			t = wx[loc-1:loc+1]
			spc = Splin_coeffs(t,wy[loc-1:loc+1])
			spcp = Splin_coeffs(t,Splin_eval(t,spc,der=1))
			res = Splinroot(spcp,wx[[loc-1,loc+1]],der=1,stat=stat)
			stat = stat[0]
		endif
		if not stat and keyword_set(ser) then serfl = 1 else gofl = 0
	endwhile

	return, FPU_fix(res[0])
end
