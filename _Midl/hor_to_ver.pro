Pro Hor_to_ver, eran = ern, first = fir, second = sec, negative = neg, show=sho

	on_error, 1

	mult = 1d6*!dpi/180
	ene = Make_grid(double(ern),step=0.1,dim=n)
	n = n[0]
	if keyword_set(neg) then begin
		sgn = -1
		pm = '-' 
	endif else begin
		sgn = 1
		pm = '+'
	endelse

	ang = Bragg_angle(ene=ene,cry='ge',ind=sec) + $
		sgn*Bragg_angle(ene=ene,cry='ge',ind=fir) - 45d
	spc = Splin_coeffs(ene,ang)
	zene = Splinroot(spc)

	fang = Bragg_angle(ene=zene,cry='ge',ind=fir,dar=fdar)
	sang = Bragg_angle(ene=zene,cry='ge',ind=sec,dar=sdar)

	print
	print, sec, pm, fir, form='("		(",3i1,")  ",a1,"  (",3i1,")")'
	print
	print, '	Optimal energy	= ' + string(zene,form='(f7.3," keV")')
	print
	print, '	First angle 	= ' + string(fang,form='(f7.3," deg.")')
	print, '	Second angle 	= ' + string(sang,form='(f7.3," deg.")')
	print
	print, '	First Dar. wid	= '+string(mult*fdar,form='(f5.0," microrad")')
	print, '	Second Dar. wid	= '+string(mult*sdar,form='(f5.0," microrad")')
	print

	if keyword_set(sho) then begin
		plot, ene, ang
		plots, [zene,0], psym=8, symsize=1.5, col=!pcol.red
	endif

	return
end