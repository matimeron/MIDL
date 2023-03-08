Function In2d_fun, var, xconst = xcn, yconst = ycn, zval = zvl, _extra = _e

;+
; NAME:
;		IN2D_FUN
; VERSION:
;		8.72
; PURPOSE:
;		Kernel function for the IN2D_INV function.  Not intended to be called
;		directly.
; CATEGORY:
;		Mathematical.
; CALLING SEQUENCE:
;		Result = IN2D_FUN( VAR, XCONST = XCN or YCONST = YCN, ZVAL = ZVL)
; INPUTS:
;	VAR
;		Numeric variable.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	XCONST											|
;		Numeric constant, interpolation X-value.	|	One and only one of
;	YCONST											|	these must be given.
;		Numeric constant, interpolation Y-value.	|
;	ZVAL
;		Numeric scalar.
;	_EXTRA
;		Formal keyword used to transfer values to imbedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Returns the value of the interpolated function, using the interpolation
;		grid stored in a common block.  If XCONST is provided, VAR serves as the
;		Y-values, else if YCONST is provided, VAR serves as the X-values.  In
;		eithercase ZVAL is subtracted from the result.
; OPTIONAL OUTPUT PARAMETERS:
;		None
; COMMON BLOCKS:
;		INTER_INFO.  See the routine IN2D_COM for details.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The X and Y inputs must be within the range covered by the interpolation
;		data.  This is usually assured by the calling routines.
; PROCEDURE:
;		Standerd interpolation using the INTER_2D routine.  Calls FPU_FIX,
;		INTER_2D and ONE_OF,from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JUL-2018 by Mati Meron.
;		Modified 10-JUL-2020 by Mati Meron.  Internal changes.
;-

	common inter_info, exs, nxs, nys, lxs, lys, dx, dy, a, bx, by, cxy
	on_error, 1

	whi = One_of(xcn,ycn,val=con)
	if n_elements(con) eq 1 then begin
		if n_elements(zvl) eq 1 then begin
			case whi of
				0	:	res = Inter_2d(con[0],var,/vec,/las,_extra=_e) - zvl[0]
				1	:	res = Inter_2d(var,con[0],/vec,/las,_extra=_e) - zvl[0]
				else:	message, 'Either X or Y must be defined!'
			endcase
		endif else message, 'Scalar Z-value required!'
	endif else message, 'Scalar constant required!'

	return, FPU_fix(res)
end