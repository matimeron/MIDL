Pro Bounce_prep, ene, step

	common fspdat, exs, erange, fspc
	on_error, 1

	if n_elements(ene) eq 3 then begin
		erange = ene[[0,2]]

		ind1 = [1,1,1]
		ene1 = Make_grid(ene[0:1],step,/step)
		dum = Bragg_angle(ene=ene1[0],cry='si',ind=ind1,eta=bw1)
		ref1 = Ref_curve(0,ene=ene1,crys='si',ind=ind1)
		crl_id2, eran=ene[0:1],step=step,tran=trn1
		trn1 = trn1[1,*]
		flu1 = Und_flux(ene1,per=27,har=3,ban=bw1,ape=[1,1],/def,/opt)
		flu1 = flu1*ref1*trn1

		ind2 = [2,2,0]
		ene2 = Make_grid(ene[1:2],step,/step)
		dum = Bragg_angle(ene=ene2[0],cry='si',ind=ind2,eta=bw2)
		ref2 = Ref_curve(0,ene=ene2,crys='si',ind=ind2)
		crl_id2, eran=ene[1:2],step=step,tran=trn2
		trn2 = trn2[1,*]
		flu2 = Und_flux(ene2,per=27,har=3,ban=bw2,ape=[1,1],/def,/opt)
		flu2 = flu2*ref2*trn2

		fspc = Splin_coeffs([ene1,ene2],[flu1,flu2],/seg)
		exs = 1
	endif else message, 'ENE needs 3 values!'

	return
end