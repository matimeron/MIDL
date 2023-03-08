Pro ID_14_gen, apersize= aps, config = cnf, slope_err= ser, reset= rst

	common id_14_pars, exs, milfac, micfac, elem, eloc, $
	filt, flth, wind, wnth, mircot, mirlen, mirrad, mirser, $
	aper, curap, filf, fils, curcnf, sunfl

	on_error, 1
	Opt_init

	exs = Default(exs,0,/dtyp)
	if keyword_set(rst) then exs = 0

	if not exs then begin
		exs = 1
		milfac = 1e3
		micfac = 1e6
		elem = ['apert','filte','fmask','vmirr','hmirr','slits', $
				'pshut','wshut','windo','sampl','backs']
		eloc = [   25.0,   25.0,   44.0,   47.0,   49.6,   51.8, $
				   51.0,   53.0,   54.2,   56.0,   58.5]
		filt = 'be'
		flth = 0.5
		wind = 'be'
		wnth = 0.5
		mircot = 'rh'
		mirlen = 1.
		mirrad = 2500.
		mirser = [2.,2.]
		aper = [0.,0.]
		curap = (filf = (fils = (curcnf = '')))
		sunfl = 0
	endif

	if n_elements(ser) ne 0 then mirser = Cast(([ser,ser[0]])[0:1],4)

	aposib = ['small','medium','large','huge']
	apsiz = [[1.5,1.5],[2.,2.],[3.,2.],[4.5,4.5]]

	if Type(aps) eq 7 then begin
		whi = Strmatch_mm(aps,aposib,3)
		if whi lt 0 then message, 'Unrecognized aperture size!'
	endif else begin
		if Streq(curap,'') then whi = 0 else whi = -1
	endelse
	if whi ge 0 then begin
		curap = aposib[whi]
		aper = reform(apsiz[*,whi])
		print
		print, '	aperture size set to [' + $
		strjoin(string(aper,form='(f5.2)'),',') + '] mm'
		print
	endif

	cposib = ['standard','other','idiotic','un23','un27','cg27','un_a']
;	fnam = $
;	['u23_el_k1p17','u27_el_k0p93','u27_el_k1p41','u27_el_k1p73','ua_m_k2p62']
	fnam = $
	['u23_up_k1p22','u27_el_k0p93','u27_el_k1p41','u27_up_k1p80','ua_m_k2p62']
	fnum = [[0,1],[0,2],[0,3],[0,0],[1,1],[3,3],[4,4]]

	if Type(cnf) eq 7 then begin
		wha = Strmatch_mm(cnf,cposib,4)
		if wha lt 0 then message, 'Unrecognized configuration'
	endif else begin
		if Streq(curcnf,'') then wha = 0 else wha = -1
	endelse
	if wha ge 0 then begin
		curcnf = cposib[wha]
		filf = $
		File_get(fnam[fnum[0,wha]],filt='dat',path=getenv('sruff_data')+'bio\')
		fils = $
		File_get(fnam[fnum[1,wha]],filt='dat',path=getenv('sruff_data')+'bio\')
		if fnum[0,wha] eq fnum [1,wha] then sunfl = 1 else sunfl = 0
		print, 'Data files are:
		print
		print, '	', filf
		print, '	', fils
		print
	endif

	return
end


