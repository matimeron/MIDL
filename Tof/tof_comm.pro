Pro Tof_comm, ex, ey, edex, edey, optic = opt, show = sho, flag= flg, nseg= ens

;+
; NAME:
;	TOF_COMM
; PURPOSE:
;	Creates and maintains the main TOF common block, TOF_VARS.
; CATEGORY:
;	TOF specific.
; CALLING SEQUENCE:
;	TOF_COMM [, parameters [, keywords]]
; INPUTS:
;	All inputs are optional.
; OPTIONAL INPUT PARAMETERS:
;    EX
;	External values of X.  Vector (first component should be 0).
;    EY
;	External values of Y.  Vector (first component should be 0).  If 
;	provided, should be same length as X, otherwise it is replaced by the 
;	vector [0,1,...1].
;    EDEX
;	External value of DEX.  Default is 0.
;    EDEY
;	External value of DEY.  Default is 0.
; KEYWORD PARAMETERS:
;    OPTIC
;	Forces calculation of optical matrices.
;    SHOW
;	If set, the values of the common block entries are displayed.
;    FLAG
;	Generic parameter.
;    NSEG
;	Accepts an integer to become the number of segments.  If specified, a
;	generic TOF is generated.  In this case EX and EY should be left 
;	unspecified.
; OUTPUTS:
;	None.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	TOF_VARS	
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Straightforward.  Uses DEFAULT and TYPE from MIDL.
; MODIFICATION HISTORY:
;	Created by Mati Meron.
;-

    common Tof_vars, x, y, dex, dey, ns, eps, alph, beth, opmat, flag, splready

    on_error, 1

    if n_elements(ens) ne 0 then begin
	if n_elements(ex) ne 0 then message, 'Overspecified!' $
	else ex = findgen(1 + ens)/ens
    endif

    if n_elements(ex) ne 0 then begin
	x = ex
	ns = n_elements(x) - 1
        if n_elements(ey) - 1 eq ns then y = ey else y = [0.,replicate(1.,ns)]
	dex = Default(edex,0.)
	dey = Default(edey,0.)
	eps = 1e-4
	datyp = Type(x) > Type(y)
	opmat = make_array(2,2,ns+1, type = datyp)
	flag = Default(flg,0)
	splready = 0
    endif

    if flag and ns gt 2 then y(ns-1) = y(ns-2) else y(ns-1) = 1.
    alph = 2*[0, (x(1:ns) - x(0:ns-1))/(y(1:ns) + y(0:ns-1))]
    beth = 0.5*[0, ((y(1:ns-1) - y(0:ns-2))/alph(1:ns-1) - $
		(y(2:ns) - y(1:ns-1))/alph(2:ns))/y(1:ns-1),0]

    if keyword_set(opt) then begin
	opmat(*,*,0) = [[1.,0.],[0.,1.]]
	for i = 1, ns do opmat(*,*,i) = $
	opmat(*,*,i-1)#[[1.,alph(i)],[0.,1.]]#[[1.,0.],[beth(i),1.]]
    endif

    if keyword_set(sho) then begin
	print, ''
	print, 'dex = ', dex, '	dey = ', dey, '		flag = ', flag
	print, ''
	print, 'x    = ', x
	print, 'y    = ', y
	print, ''
	print, 'alph = ', alph
	print, 'beth = ', beth
	print, ''
	print, '	End-point Opmat'
	print, ''
	print, opmat(*,*,ns)
	print, ''
    endif

    return
end
