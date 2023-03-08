Pro Complexmap, v_0, v_1, v_2, v_3, v_4, v_5, v_6, v_7, xlims= xlm, ylims= ylm,$
	line = lin, color = col, arrow = aro, truasp = tra, over = ove, axes = axs,$
	 _extra = _e

;+
; NAME:
;		COMPLEXMAP
; VERSION:
;		6.3
; PURPOSE:
;		Plots a complex variable as a curve in the X (real) and Y (imag.) plane.
; CATEGORY:
;		DIsplay
; CALLING SEQUENCE:
;		COMPLEXMAP, V_0 [,V_1, ... V_7] [, keywords]
; INPUTS:
;	V_0 through V_7
;		Numeric, otherwise arbitrary, at least one should be given.  Internally
;		all inputs are treated as complex vectors.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	XLIMS
;		2-element vector, specifies the X (real) display limits.  If not given
;		default is generated internally based on the data.
;	YLIMS
;		Same as XLIMS for the Y (imaginary) display limits.
;	LINE
;		Numeric vector (scalar acceptible) specifies the line types to be used.
;		If the number of entries is less than the number of variables, types are
;		recycled.  Default is 0 (solid line).
;	COLOR
;		Numeric vector (scalar acceptible) specifies the colors to be used.  If
;		the number of entries is less than the number of variables, colors are
;		recycled.  Default is 0 (black).
;	ARRO
;		Numeric scalar, the number of arrowheads to be drawn on each curve.
;		Default is 2 (beginning and end).
;	TRUASP
;		Corrects the XLIMS or YLIMS values to yield a 1:1 aspect ratio.  See
;		BOX in MIDL for possible inputs.
;	/OVER
;		Switch.  If set, COMPLEXMAP plots over existing window with no erasing.
;	/AXES
;		Switch.  If set, X and Y axes are drawn (if within range).
;	_EXTRA
;		A formal keyword used to pass  plotting keywords.  Not to be used
;		directly.
; OUTPUTS:
;		None other than graphics output.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		All the V_i present have to be valid numeric inputs.
; PROCEDURE:
;		Straightforward.  Using ARRO, BOX and DEFAULT (and indirectly CAST,
;		ISNUM, IMAGINARY_MM and REAL_MM) from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JUL-2007 by Mati Meron.
;-

	on_error, 1
	vnam = strcompress('v_' + sindgen(8),/rem)
	wnam = strcompress('w_' + sindgen(8),/rem)

	nv = n_params()
	aro = Default(aro,2l,/dtyp) > 2
	lin = Default(lin,0,/dtyp)
	col = Default(col,0,/dtyp)
	nlin = n_elements(lin)
	ncol = n_elements(col)

	if nv gt 0 then begin
		ncheck = 0
		for k = 0l, nv-1 do begin
			dum = execute('ncheck = Isnum('+vnam[k]+')')
			if ncheck then dum = execute(wnam[k] + ' = Cast(['+vnam[k]+'],6)') $
			else message, 'Non numeric input, invalid!'
			dum = execute('xmin = min(Real_mm('+wnam[k]+'),max=xmax)')
			dum = execute('ymin = min(Imaginary_mm('+wnam[k]+'),max=ymax)')
			xlo = xmin < Default(xlo,xmin)
			xhi = xmax > Default(xhi,xmax)
			ylo = ymin < Default(ylo,ymin)
			yhi = ymax > Default(yhi,ymax)
		endfor

		if not keyword_set(ove) then begin
			wxlm = Default(xlm,[7*xlo-xhi,7*xhi-xlo]/6,/dtyp)
			wylm = Default(ylm,[7*ylo-yhi,7*yhi-ylo]/6,/dtyp)
			Box, wxlm, wylm, /bord, /upd, truasp = tra, _extra = _e
			if keyword_set(axs) then begin
				if product(wxlm) le 0 then plots, 2*wxlm, [0,0], lin=1, noclip=0
				if product(wylm) le 0 then plots, [0,0], 2*wylm, lin=1, noclip=0
			endif
		endif

		for k = 0l, nv-1 do begin
			dum = execute('x = Real_mm('+wnam[k]+')')
			dum = execute('y = Imaginary_mm('+wnam[k]+')')
			plots, x, y, line = lin[k mod nlin], col = col[k mod ncol], $
			noclip = 0, _extra = _e
			nn = n_elements(x)
			if nn gt 1 then begin
				na = aro < nn
				tloc = round((nn-1)*findgen(na)/(na-1)) > 1
				floc = tloc-1
				for l = 0l, na-1 do Arro, $
				from = [x[floc[l]],y[floc[l]]], to = [x[tloc[l]],y[tloc[l]]], $
				line= lin[k mod nlin], col= col[k mod ncol], _extra = _e
			endif
		endfor
	endif else message, 'Missing input!'

	return
end