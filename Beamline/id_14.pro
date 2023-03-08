Pro ID_14, loc, foc= foc, hang= han, vang= van, abs= abs, show= sho, $
	power = pow, bsize = bsiz, esize = spot, _extra= _e

	on_error, 1

	if keyword_set(abs) then pow = ID_14_abs(loc,foc=foc,han=han,van=van,/air,$
	sho=sho,win=win,spot=spot,stat=fst,out=powar,charsize=1.4,_extra=_e) else $
	pow = ID_14_inc(loc,foc=foc,han=han,van=van,/air,sho=sho,win=win,spot=spot,$
					stat=fst,out=powar,charsize=1.4,_extra=_e)

	bsiz = 2*win
	print
	print, 'Power		=	', pow
	print, 'Beam size	=	', bsiz
	print, 'Eff. size	=	', spot
	print
	print, 'Peak pd 	=	', max(powar[2,*,*])
	print, 'Aver. pd	=	', pow/float(product(spot))
	print, 'Pow/circ.	=	', pow/(2*total(spot))
	print
	print, 'Focus stat.	=	', fst
	print

	dum = xyfit(powar,win=win,/gauss,qua=[1,1,1],/show_fit,int=int,xstyle=1)
	print, 'Integral check	', int
	print

	return
end

