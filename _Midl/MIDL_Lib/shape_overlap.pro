Function Shape_overlap, shape0, shape1, union= unn, clean= cln, tol_mult= tml, $
	exists= exs

;+
; NAME:
;		SHAPE_OVERLAP
; VERSION:
;		8.45
; PURPOSE:
;		Finds the SHAPE (see SHAPE_VER for a definition) resulting from the
;		overlap of two given shapes.
; CATEGORY:
;		General Graphics.
; CALLING SEQUENCE:
;		Result = SHAPE_OVERLAP( SHAPE1, SHAPE2[, EXISTS = EXS])
; INPUTS:
;	SHAPE0
;		Two dimensional shape i.e. a [2,*] numeric array.
;	SHAPE1
;		Ditto
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	/UNION
;		Switch.  If set, the union of the two shapes is evaluated.  The default
;		is intersection evaluation.
; 	CLEAN
; 		Switch.  If set, spurious points and zero area loops are cleaned off the
; 		shape.
;
;		Note:	CLEAN is set on by default.  It can be disabled by explicitly
;				entering CLEAN = 0.
;	TOL_MULT
;		Numeric scalar, tolerance multiplier to be used by the function
;		SHAPE_CLEAN.  See there for details.
;	EXISTS
;		Optional output, see below.
; OUTPUTS:
;		Returns a new shape, which is the overlap of the original two shapes.
;		If UNION is set, the intersection of the two shapes is returned.
;		Important:  For the purpose of the calculation the two shapes are
;		assumed to have the same direction (even if they don't)
;		If there are no points left in the result shape, a single point (i.e.
;		2D vector) with X and Y coordinates equal to the square root of the
;		maximal floating value (machine dependent) is returned.
; OPTIONAL OUTPUT PARAMETERS:
;	EXISTS
;		The name of a variable to receive calculation status result.  Returns
;		1b if the result shape is non-empty, 0b otherwise.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Tracks the propagation of the boundary between intersection points.
; 		Calls ARRLOC, CALCTYPE, CAST, DEFAULT, LINCROSS, SHAPE_AREA,
; 		SHAPE_CLEAN, SHAPE_CLOSE, SHAPE_IN, SHAPE_VER and SIGN, from MIDL.
; MODIFICATION HISTORY:
;		Created 20-JUL-1999 by Mati Meron.
;		Checked for operation under Windows, 30-JAN-2001, by Mati Meron.
;		Modified 10-FEB-2007 by Mati Meron.  Bug fix.
;		Modified 5-SEP-2011 by Mati Meron.  Added keywords CLEAN and TOL_MULT.
;		Completely rewritten 20-FEB-2016 by Mati Meron.  Added keyword UNION.
;-

	on_error, 1
	exs = 1b
	nada = sqrt(reform(replicate((machar()).xmax,2),2,1))

	clfl = Default(cln,1,/dtyp)
	unfl = keyword_set(unn)
	typ = Calctype(shape0,shape1)

	if clfl then begin
		wsh0 = Shape_clean(Cast(shape0,4),/close)
		wsh1 = Shape_clean(Cast(shape1,4),/close)
	endif else begin
		wsh0 = Shape_close(Cast(shape0,4))
		wsh1 = Shape_close(Cast(shape1,4))
	endelse

	nd0 = Shape_ver(wsh0,len = np0)
	nd1 = Shape_ver(wsh1,len = np1)
	if nd0 ne 2 or nd1 ne 2 then begin
		message, 'Only 2-D shapes accepted!', /continue
		return, 0b
	endif

	if (np0 < np1) gt 3 then begin
		if Shape_area(wsh0) lt 0 then wsh0 = reverse(wsh0,2)
		if Shape_area(wsh1) lt 0 then wsh1 = reverse(wsh1,2)

		ncr = 0
		ic0 = (ic1 = (crp = []))
		for i0 = 0l, np0-2 do begin
			for i1 = 0l, np1-2 do begin
				if Lincross(wsh0[*,i0:i0+1],wsh1[*,i1:i1+1],cros=crl) then begin
					ncr = ncr + 1
					ic0 = [ic0,i0]
					ic1 = [ic1,i1]
					crp = [[crp],[crl]]
				endif
			endfor
		endfor

		if ncr gt 0 then begin
			typ = typ > 4
			wsh0 = wsh0[*,0:-2]
			wsh1 = wsh1[*,0:-2]
			np0 = np0-1
			np1 = np1-1
			tem = wsh0[*,1:-1] - wsh0[*,0:-2]
			plen = [0,total(sqrt(total(tem^2,1)),/cum)]
			ploc = fltarr(ncr)
			for k = 0, ncr-1 do ploc[k] = $
			plen[ic0[k]] + sqrt(total((crp[*,k] - wsh0[*,ic0[k]])^2))
			s = sort(ploc)
			ic0 = ic0[s]
			ic1 = ic1[s]
			crp = crp[*,s]
			wsh0 = shift(wsh0,0,-ic0[0])
			wsh1 = shift(wsh1,0,-ic1[0])
			ic0 = (ic0 - ic0[0] + np0) mod np0
			ic1 = (ic1 - ic1[0] + np1) mod np1
			dum = where(ic1 gt 0,ndum)
			if ndum gt 0 then begin
				tem = ic1
				tem[0:dum[-1]] = np1
				dum = where(tem eq 0,ndum)
				if ndum gt 0 then ic1[dum] = np1
			endif
			ic0 = [ic0,np0]
			ic1 = [ic1,np1]

			whi = intarr(ncr)
			for k = 0, ncr-1 do begin
				v0 = wsh0[*,(ic0[k]+1) mod np0] - wsh0[*,ic0[k] mod np0]
				v1 = wsh1[*,(ic1[k]+1) mod np1] - wsh1[*,ic1[k] mod np1]
				spin = Sign(v0[0]*v1[1] - v0[1]*v1[0])
				whi[k] = (1 + (1 -2*unfl)*spin)/2
			endfor

			res = []
			for k = 0, ncr-1 do begin
				if whi[k] then begin
					wsh = wsh1
					np = np1
					ic = ic1
				endif else begin
					wsh = wsh0
					np = np0
					ic = ic0
				endelse
				res = [[res],[crp[*,k]]]
				if ic[k+1] gt ic[k] then begin
					ind = (ic[k]+ 1+ lindgen(ic[k+1]-ic[k])) mod np
					res = [[res],[wsh[*,ind]]]
				endif
			endfor
			res = [[res],[crp[*,0]]]

		endif else begin
			in0 = Shape_in(wsh0,wsh1[*,0])
			if in0 then begin
				if unfl then res = wsh0 else res = wsh1
			endif else begin
				in1 = Shape_in(wsh1,wsh0[*,0])
				if in1 then begin
					if unfl then res = wsh1 else res = wsh0
				endif else begin
					if unfl then begin
						wsh0 = wsh0[*,1:*]
						wsh1 = wsh1[*,1:*]
						disq = make_array(np0-1,np1-1,type=typ>4)
						for i0 = 0, np0-2 do begin
							for i1 = 0, np1-2 do begin
								disq[i0,i1] = (wsh0[0,i0]- wsh1[0,i1])^2 + $
									(wsh0[1,i0]- wsh1[1,i1])^2
							endfor
						endfor
						dum = min(disq,loc)
						mloc = Arrloc(loc,size(disq))
						wsh0 = shift(wsh0,0,-mloc[0])
						wsh1 = shift(wsh1,0,-mloc[1])
						res = [[Shape_close(wsh0)],[Shape_close(wsh1)]]
					endif else begin
						res = nada
						exs = 0b
					endelse
				endelse
			endelse
		endelse
	endif else message, 'Shapes must have 3 or more points each!'

	if exs then begin
		if clfl then res = Shape_clean(res,/clo)
		res = Cast(res,typ,typ,/fix)
	endif

	return, res
end