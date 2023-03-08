Function Correct_energy, actual = act, nominal = nom

	on_error, 1
	dtet = 0.062
	crys = 'si'
	ind = [3,1,1]
	
	whi = One_of(act,nom,val=ene)
	if whi ge 0 then begin
		tang = Bragg_angle(ene=ene,crys=crys,ind=ind)
		cang = tang + (2*whi - 1)*dtet
		res = Mono_energy(cang,crys=crys,ind=ind)
	endif else message, 'Either actual or nominal energy is required!'

	return, res
end		