Function Bimorph_mat, n, cvec = cvc, default = def

;+
; NAME:
;		BIMORPH_MAT
; VERSION:
;		8.15
; PURPOSE:
;		Generates the theoretical bimorph matrix for bimorph calculations. 
; CATEGORY:
;		Bimorph mirror specific.
; CALLING SEQUENCE:
;		Result = BIMORPH_MAT(N [, CVEC = CVC, DEFAULT = DEF])
; INPUTS:
;	N
;		Integer scalar.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	CVEC
; 		Comparison vector, optional, if given must be of length N.
; 	/DEFAULT
; 		Switch.  If set, both N and CVEC are generated internally from the
; 		parameters in the common block BIMORPH_DAT.
; OUTPUTS:
; 		Returns a theoretical "bimorph response matrix" to be compared with a
; 		measured one.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		BIMORPH_DAT.  See BIMORPH_INIT for details.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Staightforward from theoretical description.  Calls BIMORPH_INIT and
; 		BIMORPH_PROJ.  Calls ISNUM from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JAN-2012 by Mati Meron.
;-

	common bimorph_dat, bexs, nseg, modln, bords, cwei, bmconst
	on_error, 1
	Bimorph_init

	defl = keyword_set(def)
	if defl then begin
		n = nseg
		cvc = modln
	endif

	if Isnum(cvc) then begin
		if n_elements(cvc) ne n then message, 'Dimensional mismatch!'
	endif else cvc = replicate(1.,n)
	wvc = n*cvc/total(cvc)
	if defl then wvc = wvc*cwei

	gvc = replicate(1.,n)
	vec = [gvc,0,-gvc]
	res = fltarr(n,n)
	for i = 0, n-1 do begin
		ivc = wvc[i]*vec[n-i:2*n-i-1]
		res[i,*] = Bimorph_proj(ivc,wvc)
	endfor

	return, res
end