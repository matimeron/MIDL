Function IDfield, gap, period, mm_units = mmu, k_values = kvl, enhanced = enh

	on_error, 1

	b0 = 3.44
	filf = 0.95
	mult = 1.2*(1 + 0.02*keyword_set(enh))
	coefs = [0.,5.08,-1.54]

	nels = [n_elements(gap),n_elements(period)]
	if min(nels) eq 1 then begin
		if nels[0] eq 1 then wgap = gap[0] else wgap = gap
		if nels[1] eq 1 then wper = period[0] else wper = period
	endif else message, 'At least one of the inputs must be a scalar!'

	rat = 1.*wgap/wper
	res = mult*filf*b0*exp(-Poleval(rat,coefs))
	if keyword_set(kvl) then begin
		if keyword_set(mmu) or max(period) ge 1 then sca = 1e-3 else sca = 1.
		res = sca*wper*res/(2*!pi*!srcon.bere)
	endif

	return, FPU_fix(res)
end