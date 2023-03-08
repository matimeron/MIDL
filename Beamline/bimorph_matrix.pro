Function Bimorph_matrix, snum, angle = ang, mir_loc = mlc, det_loc = dlc, $
	progress = prg, base = bas

;+
; NAME:
;		BIMORPH_MATRIX
; VERSION:
;		8.13
; PURPOSE:
;		Generating an empirical bimorph mirror response matrix.
; CATEGORY:
;		Bimorph mirror specific.
; CALLING SEQUENCE:
;		Result = MIRROR_MATRIX( SNUM, ANGLE = ANG [, keywords])
; INPUTS:
;	SNUM
;		List of scan numbers.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ANGLE
;		Mirror angle, in mr.
;	MIR_LOC
;		Numeric scalar or 2-element vector, location of the mirror(s) measured
;		in meters, from the source.  See the routine MIRROR_SLOPE for details.
;	DET_LOC
;		Numeric scalar, the detector location.  Mandatory, no default.
;	/PROGRESS
;		Switch.  If set, the numbers of the scans being processed are displayed
;		to the screen.
;	BASE
;		Optional output, see below.
; OUTPUTS:
;		Returns the response matrix of the bimorph mirror, i.e. the matrix each
;		column of which represents the local slopes generated per unit voltage
;		on the corresponding mirror segment.
; OPTIONAL OUTPUT PARAMETERS:
; 	BASE
;		Returns the base mirror slopes, i.e. the slopes obtained with no voltage
;		on the electrodes.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
;		BIMORPH_DAT.  See BIMORPH_INIT for details.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The scans on the list SNUM must include at least one scan corresponding
;		to no voltages on the mirror.
; PROCEDURE:
;		Gets the slopes needed from MIRROR_SLOPE (see there for details).
;		Calls BIMORPH_BIN, BIMORPH_INIT, BIMORPH_VER and MIRROR_SLOPE.
; MODIFICATION HISTORY:
;		Created 15-DEC-2011 by Mati Meron.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	common bimorph_dat, bexs, nseg, modln, bords, cwei, bmconst
	on_error, 1
	Bimorph_init

	prfl = keyword_set(prg)
	lis = Bimorph_ver(snum,nsc=nsc)
	whi = lonarr(nsc)
	val = fltarr(nsc)
	good = replicate(1,nsc)
	for i = 0, nsc-1 do begin
		vals = fildat.scan[lis[i]].mirval
		check = vals ne 0
		dum = where(check, ndum)
		case ndum of
			0	:	begin
						whi[i] = nseg
						val[i] = 0
					end
			1	:	begin
						whi[i] = dum[0]
						val[i] = vals[dum[0]]
					end
			else:	begin
						message, 'Scan ' + string(lis[i],form='(i0)') + $
						' is inappropriate', /con
						good[i] = 0
					end
		endcase
	endfor
	dum = where(good,ndum)
	if ndum gt 0 then begin
		lis = lis[dum]
		whi = whi[dum]
		val = val[dum]
	endif else message, 'No remaining appropriate scans'

	dum = where(whi eq nseg, ndum)
	if ndum gt 0 then begin
		if prfl then print
		for j = 0, ndum-1 do begin
			if prfl then print, nseg, lis[dum[j]]
			dat = Mirror_slope(lis[dum[j]],angle=ang,mir_loc= mlc,det_loc=dlc)
			bdat = Bimorph_bin(dat,/ren)
			if j eq 0 then begin
				zvl = reform(bdat[0,*])
				sbas = reform(bdat[1,*])
			endif else sbas = sbas + reform(bdat[1,*])
		endfor
		if prfl then print
		sbas = sbas/ndum
		bas = transpose([[zvl],[sbas]])
	endif else message, 'Missing base!'

	res = fltarr(nseg,nseg)
	for i = 0l, nseg-1 do begin
		dum = where(whi eq i, ndum)
		if ndum gt 0 then begin
			for j = 0, ndum-1 do begin
				if prfl then print, i, lis[dum[j]]
				dat = Mirror_slope(lis[dum[j]],angle=ang,mir_lo=mlc,det_lo=dlc)
				bdat = Bimorph_bin(dat,/ren)
				if j eq 0 then seg = reform(bdat[1,*]) $
				else seg = seg + reform(bdat[1,*])
			endfor
			seg = seg/ndum - sbas
			res[i,*] = seg/val[dum[0]]
		endif else message, 'Missing segment #' + string(i,form='(i0)'), /con
	endfor
	if prfl then print

	return, res
end