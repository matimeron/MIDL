Pro CRL_tab, dat, val_ref = var, len_ref = ler, rat = rat, paclen = pac, $
	nlens = nle, enevals = env

	on_error, 1

	evals = CRL_locs(dat)
	nlocs = n_elements(evals)
	dum = min(abs(evals-var),rloc)
	if rat le 1 then ncyc = ceil(1./rat) else message, 'RAT must be <= 1!'
	roff = ((rloc + ncyc - 1)/ncyc)*ncyc

	nlocs = n_elements(evals)
	ind = indgen(nlocs)
	flen = (slen = intarr(nlocs))
	for i = 0, ncyc-1 do begin
		dum = where( ((ind - rloc + roff) mod ncyc) eq i, /null)
		flen[dum] = ler - roff/ncyc + (ind[dum] - rloc + roff)/ncyc
		slen[dum] = i
	endfor

	tevl = string(evals,form='(f6.3)')
	tlen = string(flen,form='(i0)') + ', ' + string(slen, form='(i0)')

	tabulate, tlen, tevl, form = ['a6', 'a6']

	wpac = Default(pac,nlocs,/dtyp)
	nco = ceil(1.*nlocs/wpac)
	nle = (env = strarr(nco,wpac))
	for i = 0, nco-2 do begin
		nle[i,*] = tlen[i*wpac:(i+1)*wpac-1]
		env[i,*] = tevl[i*wpac:(i+1)*wpac-1]
	endfor
	if (nco-1)*wpac lt nlocs then begin
		nle[nco-1,0:nlocs-(nco-1)*wpac-1] = tlen[(nco-1)*wpac:nlocs-1]
		env[nco-1,0:nlocs-(nco-1)*wpac-1] = tevl[(nco-1)*wpac:nlocs-1]
	endif

	return
end