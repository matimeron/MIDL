Function Peaks, x, y, width = wid, sigma = sig, amplitude = amp, number = num,$
	_extra = _e

	on_error, 1
	doub = Isnum(x,/doub) or Isnum(y,/doub)
	sinf = machar(double = doub)
	erv = sinf.xmax

	brac = Peak_scan(x,y,width=wid,number=num,second=sdy,_extra=_e)

	if num gt 0 then begin
		loc = fltarr(num)
		sig = loc
		amp = loc
		for i = 0, num-1 do begin
			loc[i] = Peak_loc(x,sdy,brac[i,*],width=wid,sig=s,amp=a)
			sig[i] = s
			amp[i] = a
		endfor
	endif else begin
		loc = erv
		sig = erv
		amp = erv
	endelse

	return, loc
end
