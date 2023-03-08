Pro UN_info, file = fname, unstruct = exu, details = u

	common un_consts, hcove, alpha, oovee, ecf

	on_error, 1
	u = UN_struct()

	rnr = One_of(exu,fname)
	if rnr then begin
		fname = $
		file_get(fname,filt='dat',path=getenv('sruff_data'),/pick,stat=stat)
		if stat then begin
			openr, datun, fname, /get_lun, /block
			readu, datun, u
			free_lun, datun
		endif else message, 'Cannot find file!'
	endif else u = exu

	print
	print, 'name        =  ', strtrim(u.name,2)
	print, 'k           =  ', u.k, format = '(a,f5.2)'
	print, 'lambda      =  ', 100*u.lamb, format = "(a,f5.2,' cm')"
	print, 'gamma       =  ', u.gamm, format = '(a,f7.1)'
	print, '# periods   =  ', u.nper, format = '(a,i3)'
	print

	etem = ecf*hcove*u.gamm^2/u.lamb
	print, 'E_harmonic  =  ', 2*etem/(1 + 0.5*u.k^2), format = "(a,f6.3,' keV')"
	print, 'E_critical  =  ', 1.5*etem*u.k, format = "(a,f6.3,' keV')"
	print

	print, 'sigma       =  ', 1e3*u.rsig, $
	format = "(a,'[ ',f6.1,', ',f6.1,' ]',' micron')"
	print, 'sigma_xang  =  ', 1e3*u.asig, $
	format = "(a,'[ ',f6.1,', ',f6.1,' ]',' microradian')"

	alims = [u.ganx(2*u.nxy(0)),u.gany(2*u.nxy(1))]
	print, 'ang_range   =  ', 1e3*alims, $
	format = "(a,'[ ',f6.1,', ',f6.1,' ]',' microradian')"
	print, '# points    =  ', u.nxy, format = "(a,i4,', ',i4)"
	print

	hstr = ['harmonics  =  ','                 ']
	tem = u.harms(1:u.nh)
	dtem = tem - [tem(0) - 1,tem]
	w = where(dtem gt 1, nw)
	if nw eq 0 then w = [0,u.nh] else w = [0,w,u.nh]
	for i = 0l, nw do begin
		low = tem(w(i))
		high = tem(w(i+1) - 1)
		if low eq high then print, hstr(i gt 0), low, format = '(a,i3)' else $
		print, hstr(i gt 0), low, high, format = "(a,i3,' - ',i3)"
	endfor
	print

	return
end