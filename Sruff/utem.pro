Function utem, sli, file, wait = wai, _extra = _e

	on_error, 1
	res = 0.*sli
	for i = 0, n_elements(sli)-1 do begin
		und_slice, 0.1,30,file=file, ban=1.4e-4,/sme,sli=sli[i],$
			win=0.5*[2.4,1.2],tflu=flu, _extra = _e
			res[i] = flu
			wait, Default(wai,0.,/dtyp)
	endfor

	return, res
end

	