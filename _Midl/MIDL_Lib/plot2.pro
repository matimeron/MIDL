Pro Plot2, x, l0 = l0, l1 = l1, r0 = r0, r1 = r1, yrange= yrn, colors= cls, $
	title = tit, xtitle = xtit, ytitle = ytit, no_restore = nrs, _extra = _e

;+
; NAME:
;		PLOT2
; VERSION:
;		8.03
; PURPOSE:
;		Plots 2 or more different functions, with 2 different ranges, on the 
;		same plot.
; CATEGORY:
;		Plotting.
; CALLING SEQUENCE:
;		PLOT2, [X], L0 = L0 [, L1 = L1], R0 = R0 [, R1 = R1] [, keywords] 
; INPUTS:
; 	X
; 		A numeric vector, serving as the X-coordinate of the plot.  If not
; 		given, it is generated internally.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	L0
; 		Numeric vector, set of Y values corresponding to the left Y-axis.
; 	L1
; 		Optional additional set of Y values for the left Y-axis.
; 	R0
; 		Numeric vector, set of Y values corresponding to the right Y-axis.
; 	R1
; 		Optional additional set of Y values for the right Y-axis.
;
;		Note:	If only one set of Y values corresponds to the right Y-axis,
;				it can be given either as L0 or L1, and same applies for the
;				right Y-axis.
; 	YRANGE
; 		Similar to the graphic keyword YRANGE.  Can be entered as a 2 or 4 
; 		element vector.  If given as 2 element vector, it applies to the first
; 		(left) y-axis, for Y0.  If given as 4 element vector, the first 2 values
; 		apply to Y0 and the next 2 to Y1.
; 		
; 		Note:	A pair of 0 values is the same as "not given".  Thus if YRANGE
; 				is given as [0,0,3,20], IDL will set the Y-range for Y0 
; 				automatically, and [3,20] will be the range for Y1.
;	COLORS
;		If given as a vector of 2 or more elements, the first 2 entries serve
;		as plot colors for Y0, Y1, else the plot colors are generated internally
;	TITLE
;		Same as PLOT TITLE.
;	XTITLE
;		Same as PLOT XTITLE.
;	YTITLE
;		Same as PLOT YTITLE, however, may be given as a 2-element character
;		vector, in which case the entries serve as Y-titles for the first and
;		second Y-input.  If given as a scalar, there is no second Y-title.
;	/NO_RESTORE
;		Switch.  If set, prevents restoring all plot parameters to defaults 
;		after plotting.  Useful if overplotting additional data is intended.
;		Use with care!!!
;	_EXTRA
;		A formal keyword used to pass all plotting keywords.  Not to be used
;		directly.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None, unless the keyword /NO_RESTORE is used, in which case subsequent
;		plots may be affected.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls HOW_MANY, ISNUM and PLVAR_KEEP from MIDL.
; MODIFICATION HISTORY:
;		Created 5-MAR-2011 by Mati Meron.
;		Modified 15-MAR-2011 by Mati Meron.  Added keywords YRANGE and 
;		NO_RESTORE.
;		Rewritten 15-MAR-2011 by Mati Meron.
;-

	on_error, 1

	case How_many(fir=l0,sec=l1,whi=lwhi) of
		0	:	message, 'Missing "left" data!'
		1	:	begin
					if lwhi eq 1 then wl0 = l0 else wl0 = l1
					lxn = n_elements(wl0)
					ll = [min(wl0,max=max),max]
					lnum = 1
				end
		2	:	begin
					wl0 = l0
					wl1 = l1
					lxn = n_elements(wl0) > n_elements(wl1)
					ll = [min(wl0,max=max0) < min(wl1,max=max1), max0 > max1]
					lnum = 2
				end
	endcase

	case How_many(fir=r0,sec=r1,whi=rwhi) of
		0	:	message, 'Missing "right" data!'
		1	:	begin
					if rwhi eq 1 then wr0 = r0 else wr0 = r1
					rxn = n_elements(wr0)
					rr = [min(wr0,max=max),max]
					rnum = 1
				end
		2	:	begin
					wr0 = r0
					wr1 = r1
					rxn = n_elements(wr0) > n_elements(wr1)
					rr = [min(wr0,max=max0) < min(wr1,max=max1), max0 > max1]
					rnum = 2
				end
	endcase

	if Isnum(x) then wx = x else wx = findgen(lxn > rxn)
	if n_elements(cls) gt 1 then cols = cls else cols= [!pcol.dgreen,!pcol.blue]
	case n_elements(yrn) of
		0	:	wyrn = [0,0,0,0]
		2	:	wyrn = [yrn,0,0]
		4	:	wyrn = yrn
		else:	message, 'Illegal YRANGE input, only 2 or 4 values allowed!'
	endcase	
	case n_elements(ytit) of
		0	:	wytit = ['','']
		1	:	wytit = [ytit,'']
		else:	wytit = ytit
	endcase

	xx = [min(wx,max=max),max]
	Plvar_keep, act = 'sav'
	!x.margin = [10,10]
	plot, xx, ll, /nodata, ystyle= 4, yran= wyrn[0:1], tit= tit, xtit= xtit, $
	_extra = _e
	oplot, wx, wl0, col = cols[0]
	if lnum gt 1 then oplot, wx, wl1, col = cols[0], line = 5
	axis, yaxis = 0, ytit = wytit[0], col = cols[0]
	plot, xx, rr, /nodata, /noerase, xstyle= 4, ystyle= 4, yran= wyrn[2:3], $
	_extra = _e
	oplot, wx, wr0, col = cols[1]
	if rnum gt 1 then oplot, wx, wr1, col = cols[1], line = 5
	axis, yaxis = 1, ytit = wytit[1], col = cols[1]
	if not keyword_set(nrs) then Plvar_keep, act = 'res'

	return
end