Pro Mono_offsets, energy = ene, angle = ang, loff = lof, toff = tof, $
	crystal = crs, index = ind

	on_error, 1

	wcrs = Default(crs,'Si',/dtyp)
	wind = Default(ind,[1,1,1],/dtyp)

	if How_many(fir=ene,sec=ang,thi=lof,fou=tof,whi=whi) eq 2 then begin
		case whi of
			5	:	begin
						wene = ene
						wang = Bragg_angle(ene=wene,crys=wcrs,ind=wind,eta=bw)
						wlof = lof
						wtof = wlof*tan(!dtor*2*wang)
					end
			9	:	begin
						wene = ene
						wang = Bragg_angle(ene=wene,crys=wcrs,ind=wind,eta=bw)
						wlof = tof/tan(!dtor*2*wang)
						wtof = tof
					end
			6	:	begin
						wene = Mono_energy(ang,crys=wcrs,ind=wind)
						wang = Bragg_angle(ene=wene,crys=wcrs,ind=wind,eta=bw)
						wlof = lof
						wtof = wlof*tan(!dtor*2*wang)
					end
			10	:	begin
						wene = Mono_energy(ang,crys=wcrs,ind=wind)
						wang = Bragg_angle(ene=wene,crys=wcrs,ind=wind,eta=bw)
						wlof = tof/tan(!dtor*2*wang)
						wtof = tof
					end
			12	:	begin
						tang = !radeg/2*atan(tof,lof)
						wene = Mono_energy(tang,crys=wcrs,ind=wind)
						wang = Bragg_angle(ene=wene,crys=wcrs,ind=wind,eta=bw)
						wlof = lof
						wtof = tof
					end
			else:	message, "Something is wrong here!'
		endcase
		dum = Bragg_angle(ene=wene,crys=wcrs,ind=[1,1,1],eta=rbw)
	endif else $
	message, 'Two inputs are needed, Energy-Angle combination excluded!'

	print
	print, wcrs, wind, form = '("	",a," (",3i0,")")
	print
	print, wene, wang, $
	form = '("	Energy = ",f6.3," keV; Angle = ",f6.3," deg.")' 
	print, bw, bw/rbw, $
	form='("	Bandwidth = ",e9.2,"  (",f5.3," rel. to (111))")
	print
	print, wlof, form = '("	Longitudinal offset	= ",f6.3,"m")'
	print, wtof, form = '("	Transverse offset	= ",f6.3,"m")'
	print

	return
end