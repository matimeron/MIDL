Function Bimorph_bin, dat, renorm = ren, stat = sta

;+
; NAME:
;		BIMORPH_BIN
; VERSION:
;		8.15
; PURPOSE:
;		Bins mirror slope measurements into a fitting format. 
; CATEGORY:
;		Bimorph mirror specific.
; CALLING SEQUENCE:
;		Result = BIMORPH_BIN(DAT [, RENORM = REN, STAT = STA)
; INPUTS:
;	DAT
;		Mirror slope data in a [2,N] or [3,N] format.  Mandatory
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	/RENORM
; 		Switch.  If set, the binned data is renormalized to yield zero average
; 		slope.
; 	STAT
; 		Optional output, see below.
; 	/DEFAULT
; 		Switch.  If set, both N and CVEC are generated internally from the
; 		parameters in the common block BIMORPH_DAT.
; OUTPUTS:
; 		Returns binned slope data (with one value per mirror segment), 
; 		optionally normalizes to yield zero average slope.
; OPTIONAL OUTPUT PARAMETERS:
;	STAT
;		Returns 1 if DAT contains slope values for every segment of the mirror,
;		0 otherwise.
; COMMON BLOCKS:
;		BIMORPH_DAT.  See BIMORPH_INIT for details.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Staightforward.  Calls BIMORPH_INIT and BIMORPH_PROJ.
; MODIFICATION HISTORY:
;		Created 15-JAN-2012 by Mati Meron.
;-

	common bimorph_dat, bexs, nseg, modln, bords, cwei, bmconst
	on_error, 1
	Bimorph_init

	loc = dat[0,*]
	val = dat[1,*]
	slp = (zvl = fltarr(nseg))
	for i = 0, nseg-1 do begin
		dum = where(loc ge bords[i] and loc lt bords[i+1], ndum)
		if ndum gt 0 then begin
			zvl[i] = total(loc[dum])/ndum
			slp[i] = total(val[dum])/ndum
		endif else zvl[i] = (slp[i] = -1e4)
	endfor
	if keyword_set(ren) then slp = Bimorph_proj(slp,modln)
	sta = min(slp) gt -1e4

	return, transpose([[zvl],[slp]])
end