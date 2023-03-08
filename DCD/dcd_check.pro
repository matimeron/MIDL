Pro DCD_check, ene, first_err = fer, second_err = ser, relative = rel

	on_error, 1

	typ = Calctype(ene,0.)
	phi1 = Bragg_angle(ene=1d*ene,crys='ge',ind=[1,1,1],dar=dar1,/rad)
	phi2 = Bragg_angle(ene=1d*ene,crys='ge',ind=[2,2,0],dar=dar2,/rad)

	case n_elements(fer) of
		0	:	fax = [0,0,1d]
		2	:	fax = [fer,1d]
		else:	message, 'Error needs 2 elements or none!'
	endcase

	case n_elements(ser) of
		0	:	sax = [0,0,1d]
		2	:	sax = [ser,1d]
		else:	message, 'Error needs 2 elements or none!'
	endcase

	x = [1d,0,0]
	n1 = Mat_rot(-phi1,ax=fax)##x
	n2 = Mat_rot(!dpi+phi2-2*phi1,ax=sax)##x
	v0 = [0,1d,0]
	v1 = Mat_refl(n1)##v0

	anerr1 = (Vinp(v0,n1) + sin(phi1))/cos(phi1)
	anerr2 = (Vinp(v1,n2) + sin(phi2))/cos(phi2)
	if abs(anerr1) lt 2*Toler(/doub) then anerr1 = 0d
	if abs(anerr2) lt 2*Toler(/doub) then anerr2 = 0d

	if keyword_set(rel)then begin
		anerr1 = anerr1/dar1
		anerr2 = anerr2/dar2
	endif

	print
	print, Cast(anerr1,typ,typ), Cast(anerr2,typ,typ)
	print

	return
end