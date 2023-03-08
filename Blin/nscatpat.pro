Pro NScatpat, r, distance= dst, size= dsz, center= cnt, radians = rad, $
	alp = alp, bet = bet, dth = dth, lam = lam, ene = ene, k = kvl, _extra= _e

	on_error, 1

	n = 512


	xzgrid = Make_grid(0.5*dsz*[[-1,1],[-1,1]],[n,n])
;	x = reform(xzgrid[0,*,*])
;	z = reform(xzgrid[1,*,*])
	cnt = Default(cnt,[0.,0.],/dtyp)

	walp = alp
	wbet = Default(bet,alp)
	wdth = Default(dth,0.)

	qvl = CCD_coo_conv(xzgrid,dist=dst,cent=cnt,radians=rad,/qval,/sign, $
			alp=walp,bet=wbet,dth=wdth,lam=lam,ene=ene,k=kvl,_extra=_e)

	q = reform(sqrt(qvl[0,*,*]^2 + qvl[1,*,*]^2))

	ff = Sp_form_factor(q,r)
	ffq = Img_trig(ff,qvl,xtr=qxy,ytr=qz)

	Display_mm, ffq, qxy, qz, _extra = _e
;	Display_mm, ff, xzgrid, /yrev, _extra=_e
	return
end