Function Bimorph_fit, mat, default = def, fit_par = fpr, _extra= _e

;+
; NAME:
;		BIMORPH_FIT
; VERSION:
;		8.13
; PURPOSE:
;		Fits a measured bimorph response matrix to a theoretical one. 
; CATEGORY:
;		Bimorph mirror specific.
; CALLING SEQUENCE:
;		Result = BIMORPH_FIT( MAT [, CVEC = CVC, DEFAULT = DEF])
; INPUTS:
;	MAT
;		Measured response matrix, see BIMORPH_MATRIX for details.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	/DEFAULT
; 		Switch.  Unless explicitly set to zero, instructs BIMORPH_MAT to
; 		generate the theoretical response matrix using the default setting.
; 	FIT_PAR
; 		Optional output, see below.
; OUTPUTS:
; 		Returns a theoretical "bimorph response matrix" multiplied by a 
; 		normalization constant established through a fit to the measured matrix.
; OPTIONAL OUTPUT PARAMETERS:
;	FIT_PAR
;		Returns the fit parameter (scalar).
; COMMON BLOCKS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Simple fit.  Calls BIMORPH_FUN and BIMORPH_MAT.  Calls ARREQ, DEFAULT 
; 		and QND_EXT, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-NOV-2011 by Mati Meron.
;-

	on_error, 1

	wdef = Default(def,1)
	dim = size(mat,/dim)
	cmat = Bimorph_mat(default=wdef,_extra=_e)

	if Arreq(dim,size(cmat,/dim)) then n = dim[0] $
	else message, 'Dimensional mismatch!'

	xini = total(total(mat,1)/total(cmat,1))/n
	fpr = (QND_ext('Bimorph_fun',x_ini=xini,step=abs(xini),/min,/fine,$
		mat=mat,cmat=cmat,_extra=_e))[0]

	return, fpr*cmat
end