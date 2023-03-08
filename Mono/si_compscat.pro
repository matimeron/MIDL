Function Si_compscat, ene, angle= ang, thick= thi, radians= rad, degrees= deg, $
	lo_est = les

	common compstuff, exs, lspl, hspl

	on_error, 1
	if Default(exs,0) eq 0 then begin
		filename = getenv('mono_data') + 'compton_ratios.sav'
		restore, filename
		exs = 1
	endif

	if keyword_set(les) then spli = lspl else spli = hspl
	kfac = exp(Splin_eval(alog(ene),spli))
	if (One_of(deg,rad) > 0) eq 0 then amult = !dtor else amult = 1.
	eps = sin(amult*ang)

	if n_elements(thi) eq 1 then zet = thi/eps*0.1*Abs_coeff(ene,elem='si')
	res = kfac/2*Comprob(eps,zet)

	return, res
end