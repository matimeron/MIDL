Function Walp, r0, lamb, count= cnt, ramp= rmp, power= pow, full= fls, hd= hds

	on_error, 1

	bas = 32l
	if keyword_set(fls) then bas = bas*2
	if keyword_set(hds) then mult = [25,16] else mult = [20,16]
	xysiz = bas*mult
	xylim = xysiz-1d

	xyimg = make_array(3,xysiz[0],xysiz[1],typ=5)
	xyimg[0:1,*,*] = Make_grid([[0d,xylim[0]],[0d,xylim[1]]],xysiz)

	k = 2*!pi/lamb
	x = reform(xyimg[0,*,*])
	y = reform(xyimg[1,*,*])

	n = Default(cnt,1)
	cenx = xylim[0]*randomu(s,n)
	ceny = xylim[1]*randomu(s,n)
	if Isnum(rmp) then amp = (rmp + (1-rmp)*randomu(s,n))^Default(pow,1) $
	else amp = replicate(1,n)

	for i = 0l, n-1 do begin
		r = sqrt((x-cenx[i])^2 + (y-ceny[i])^2)
		xyimg[2,*,*] = xyimg[2,*,*] + amp[i]*cos(k*r)/sqrt(r^2 + r0^2)
	endfor
	xyimg[2,*,*] = (r0*xyimg[2,*,*])^2

	return, xyimg
end