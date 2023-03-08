Function Bimorph_ver, snum, nscans = nsc

;+
; NAME:
;		BIMORPH_VER
; VERSION:
;		8.18
; PURPOSE:
;		Finds Bimorph mirror scans.
; CATEGORY:
;		Bimorph mirror specific.
; CALLING SEQUENCE:
;		Result = BIMORPH_VER( SNUM [, NSCANS = NSC])
; INPUTS:
;	SNUM
;		Scan number or a list of scan numbers, in any form acceptable by
;		SCAN_LIST_VER.  If not given or set to 0, translates to "all scans in 
;		the file".
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	NSCANS
; 		Optional output, see below.
; OUTPUTS:
; 		Returns a list of numbers of scans (from the original list SNUM) which 
; 		are bimorph mirror scans.
; OPTIONAL OUTPUT PARAMETERS:
; 	NSCANS
; 		Returns the number of mirror scans found.
; COMMON BLOCKS:
;		SPEC_FILE.  See SPEC_FILE_INFO for details.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Straightforward search for scans with MIRFL set and no missing frames.
;		Calls SCAN_COLUMN, SCAN_PD_FRAMES and SPEC_FILE_CHECK.  Calls DEFAULT 
;		and DIF, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JAN-2012 by Mati Meron.
;		Modified 5-)CT-2012 by Mati Meron.  Internal changes.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1
	eps = 1e-4

	Spec_file_check, Default(snum,0),nsc=nsc,lis=lis,_extra=_e
	if nsc eq 0 then message, 'No scans present!'
	stat = fildat.scan[lis].stat
	mirfl = fildat.scan[lis].mirfl
	pdstat = fildat.scan[lis].pdstat
	dum = where(stat eq 1 and mirfl eq 1 and pdstat eq 7, nsc)
	if nsc gt 0 then begin
		lis = lis[dum]
		good = replicate(1,nsc)
		for i = 0, nsc-1 do begin
			sparf = Scan_column(lis[i],0)
			if Streq(fildat.scan[lis[i]].stype,'a2scan') $
			then spars = Scan_column(lis[i],1) else spars = sparf
			dum = where((abs(Dif(sparf,/lin))>abs(Dif(spars,/lin))) lt eps,ndum)
			if ndum eq 0 then begin
				ddum = Scan_PD_frames(lis[i],nframes=nfr,/ver)
				if nfr ne n_elements(sparf) then good[i] = 0
			endif else good[i] = 0
		endfor
		dum = where(good eq 1, nsc)
		if nsc eq 0 then begin
			message, 'No valid scans present!', /con
			lis = -1l
		endif else lis = lis[dum]
	endif else begin
		message, 'No valid scans present!', /con
		lis = -1l
	endelse

	return, lis
end