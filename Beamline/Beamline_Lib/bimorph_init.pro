Pro Bimorph_init

;+
; NAME:
;		BIMORPH_INIT
; VERSION:
;		8.15
; PURPOSE:
;		Creates the common block BIMORPH_DAT used for bimorph calculation.
; CATEGORY:
;		Bimorph mirror specific.
; CALLING SEQUENCE:
;		BIMORPH_INIT
; INPUTS:
; 		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 		None.
; OUTPUTS:
; 		None.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		BIMORPH_DAT.  Contains:
;			BEXS	-	Scalar flag, set to 1 when the common block is defined.
;			NSEG	-	Integer scalar, the number of bimorph segments.
;			MODLN	-	Vector, the lengths of consecutive bimorph segments.
;			BORDS	-	Vector, locations of the boundaries of the segments.
;			CWEI	-	Vector of weights for the segments.
;			BMCONST	-	Bimorph constants in units of microrad/mm/volt.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Defines the common block parameters for the 15ID bimorph mirror.
;		Calls DEFAULT from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JAN-2012 by Mati Meron.
;-

	common bimorph_dat, bexs, nseg, modln, bords, cwei, bmconst
	on_error, 1

	if Default(bexs,0) eq 0 then begin
		modl = [25,25,25,37.5,37.5,37.5,37.5,75]
		modln = [reverse(modl),modl]
		tcmodl = total(modl,/cum)
		bords = [-reverse(tcmodl),0,tcmodl]
		nseg = 2*n_elements(modl)
		cwei = replicate(1.,nseg)
		cwei[0] = (cwei[nseg-1] = 0.5)
		bmconst = -5.1e-4
		bexs = 1
	endif

	return
end