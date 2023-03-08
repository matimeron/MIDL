Function CRL_find, show = sho, energy = ene, hslit= hsl, count= cnt, null= nul,$
	_extra= _e

;+
; NAME:
;		CRL_FIND
; VERSION:
;		8.72
; PURPOSE:
;		Locates within a SPEC file groups of CRL test scans.
; CATEGORY:
;		SPEC file processing.
; CALLING SEQUENCE:
;		Result = CRL_FIND ([ keywords])
; INPUTS:
; 		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/SHOW
;		Switch.  If set results is printed to the screen (in addition to being
;		returned).
;	ENERGY
;		Optional output, see below.
;	HSLIT
;		Optional output, see below.
;	COUNT
;		Optional output, see below.
;	/NULL
;		Switch.  If set and COUNT = 0 (i.e. no scans found), the output is set
;		to !NULL.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.  All the keywords of SPEC_FILE_INFO, SCAN_FIND and
;		TABULATE can be used.
; OUTPUTS:
;		Returns a list of the CRL test scans (i.e. Beam-size scans) present in
;		the currently loaded SPEC file, as a character array. If nothing is
;		found, returns a null string, or !NULL if /NULL is set.
; OPTIONAL OUTPUT PARAMETERS:
; 	ENERGY
; 		Returns a list of the energies corresponding to the scans in the output.
; 	HSLIT
; 		Returns the horizontal slit (S0) opening for the scans in the output.
;	COUNT
;		Returns the number of scans found.
;	/NULL
;		Switch.  If set and COUNT = 0 (i.e. no scans found), the output is set
;		to !NULL.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Uses SPEC_FILE_INFO when needed, and SCAN_FIND and
;		SCAN_PAR_READ to extract the needed data.  Also calls FLTROUND and
;		TABULATE from MIDL.
; MODIFICATION HISTORY:
;		Created 5-JAN-2020 by Mati Meron.
;		Modified 20-MAR-2020 by Mati Meron.  Added keyword HSLIT.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop

	on_error, 1

	Spec_file_info, _extra = _e
	lis = Scan_find(typ='Bsize',count=cnt,null=nul)
	if keyword_set(sho) then print
	if cnt gt 0 then begin
		ene = reform(Scan_par_read(lis,'en'))
		hsl = Fltround(total((fildat.scan[lis].sl_val[*,0])[2:3,*],1),dec=2)
		if keyword_set(sho) then Tabulate, lis, ene, hsl, $
		head= ['Scan #', 'Energy', 'Slit'], form=['i6','f7.3','f5.2'], _extra=_e
	endif else begin
		ene = !null
		if keyword_set(sho) then print, 'Nothing found!'
	endelse

	return, lis
end