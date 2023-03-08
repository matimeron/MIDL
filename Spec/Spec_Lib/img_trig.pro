Function Img_trig, img, x, y, xuse= xus, yuse= yus, xsmall= xsm, ysmall= ysm, $
	last = las, minimal = mnl, strict = str, xtr = rtx, ytr = rty

;+
; NAME:
;		IMG_TRIG
; VERSION:
;		8.44
; PURPOSE:
;		Image processing.
; CATEGORY:
;		Display.
; CALLING SEQUENCE:
;		Result = IMG_TRIG(IMG, X, Y, XTR = RTX, YTR = RTY)
; INPUTS:
;	IMG
;		A two dimensional numeric array.  Mandatory.
;	X
;		The X coordinates of the image points.  Mandatory (unless /LAST is set).
;		Three possibilities:
;
;		1)	Numeric vector of length equal to the first dimension of IMG.
;		2)	A 2D numeric array of same dimensions as IMG.
;		3)	A 3D numeric array of dimensions [2,M,N] where [M,N] are the
;			dimensions of IMG.  In this case X[0,*,*] is used as X and [X,1,*,*]
;			as Y, internally.
;	Y
;		The Y coordinates of the image points.  Mandatory for X of type 1-2
;		(unless /LAST is set), forbidden otherwise.  Two possibilities:
;
;		1)	Numeric vector of length equal to the second dimension of IMG.
;		2)	A 2D numeric array of same dimensions as IMG.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	XUSE
; 		An optional vector of X values, if provided it'll be used as the X
; 		coordinates for the output image.  Doesn't have to be equispaced.  The
; 		list of values will be trimmed, as needed, to fit within the range 
; 		established by X (see above).  Also, unless the keyword STRICT (see
; 		below) is set, the list may be supplemented with the lower and/or upper
; 		end point of the range of X, if those fall between consecutive points of
; 		XUSE.
; 	YUSE
; 		Same as XUSE, for the Y coordinate.
; 	/XSMALL
; 		Switch.  If set, the new X-coordinates are created with	| XUSE and
; 		a spacing equal to the smallest spacing present in the	| XSMALL are 
; 		original X-coordinates.  Note that this may change the	| mutually
; 		number of points.										| exclusive.
; 	/YSMALL														| Same for YUSE
; 		Switch.  Same as XSMALL, for the Y-coordinates.			| and YSMALL.
;	/LAST
;		Switch.  If set, the triangulation matrix and coordinate vectors from
;		the previous triangulation are reused.  This saves time in processing
;		multiple images sharing same coordinates.  If there was no previous
;		triangulation, /LAST is ignored.
;	/MINIMAL
;		Switch.  If set, the interpolation region is the largest rectangle
;		contained within the X, Y data.  Else, it is the smallest rectangle
;		containing the data.  If the region specified by X, Y is already
;		rectangular, /MINIMAL doesn't matter.
;	/STRICT
;		Switch.  If set, and if XUSE and/or YUSE are provided, prevents adding
;		the end points of the X and/or Y range to the coordinates being 
;		generated.  If neither XUSE not YUSE is provided, STRICT has no effect. 
;	XTR
;		Optional output, see below.
;	YTR
;		Optional output, see below.
; OUTPUTS:
;		Returns the result of a triangulation of IMG, corresponding to equally
;		spaced X,Y coordinates.
; OPTIONAL OUTPUT PARAMETERS:
;	XTR
;		Numeric vector of length equal to the first dimension of IMG,
;		containing the equally spaced X-coordinates used in the triangulation.
;		Note: If XSMALL is set, the length of XTR may change.
;	YTR
;		Numeric vector of length equal to the second dimension of IMG,
;		containing the equally spaced Y-coordinates used in the triangulation.
;		Note: If YSMALL is set, the length of YTR may change.
; COMMON BLOCKS:
;		TRIG_INFO.  Contains coordinate vectors and triangulation matrix from
;		the last triangulation.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward application of IDL's TRIANGULATE and TRIGRID.  Calls
;		ARREQ, CALCTYPE, CAST, DIF, FLTROUND, FPU_FIX, ISNUM, MAKE_GRID, ONE_OF,
;		SIGN, SORPURGE and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-OCT-2005 by Mati Meron.
;		Modified 25-NOV-2005 by Mati Meron.  Added keyword MINIMAL.
;		Modified 30-MAR-2010 by Mati Meron.  Internal changes.
;		Modified 15-AUG-2012 by Mati Meron.  Added keywords XSMALL and YSMALL.
;		Modified 10-APR-2013 by Mati Meron.  Internal changes.
;		Modified 1-NOV_2015 by Mati Meron.  Added keywords XUSE, YUSE and 
;		STRICT.
;-

	common trig_info, exs, wx, wy, rx, ry, tring
	on_error, 1

	wimg = reform(img)
	sim = size(wimg)
	if sim[0] ne 2 then message, 'Not an image!'

	if not (keyword_set(las) and Type(exs) gt 0) then begin
		sx = size(x)
		sy = size(y)
		typ = Calctype(0.,x,y,def=4)

		case sx[0] of
			1	:	begin
						if sy[0] eq 1 and Arreq([sx[1],sy[1]],sim[1:2]) $
						then begin
							wx = (wy = make_array(sx[1],sy[1],typ = typ))
							for j = 0l, sy[1]-1 do wx[*,j] = x
							for i = 0l, sx[1]-1 do wy[i,*] = y
						endif else message, 'Bad or inconsistent X-Y inputs!'
					end
			2	:	begin
						if Arreq([sx[0:2],sy[0:2]],[sim[0:2],sim[0:2]]) $
						then begin
							wx = Cast(x,typ)
							wy = Cast(y,typ)
						endif else message, 'Bad or inconsistent X-Y inputs!'
					end
			3	:	begin
						if Arreq(sx[1:3],sim[0:2]) and Arreq(sy,[0,0,0]) $
						then begin
							wx = Cast(reform(x[0,*,*]),typ)
							wy = Cast(reform(x[1,*,*]),typ)
						endif else message, 'Unacceptable input!'
					end
			else:	message, 'Unacceptable input!'
		endcase

		xtem = [min(wx[0,*],max=lmax),lmax,min(wx[sim[1]-1,*],max=hmax),hmax]
		ytem = [min(wy[*,0],max=lmax),lmax,min(wy[*,sim[2]-1],max=hmax),hmax]
		xtem = xtem(sort(xtem))
		ytem = ytem(sort(ytem))
		if keyword_set(mnl) then ind = [1,2] else ind = [0,3]
		xran = xtem[ind]
		yran = ytem[ind]
		wsim = sim

		chfl = 1
		case One_of(xsm,xus) of
			-1	:				
			0	:	begin
						dwx = Fltround(min(abs(Dif(wx[*,0],/lin))),dig=3)
						wsim[1] = round((max(xran,min=min) - min)/dwx) + 1
						chfl = 0
					end
			1	:	begin
						sxus = xus[Sorpurge(xus,net=nux)]
						dum = where(sxus ge xran[0] and sxus le xran[1],ndum)
						if ndum gt 0 then begin
							ux = sxus[dum]
							if not keyword_set(str) then begin
								if dum[0] gt 0 then begin
									ext = sxus[dum[0]-1:dum[0]]
									if (ext[1]-xran[0]) ge 0.5*(ext[1]-ext[0]) $
									then ux = [xran[0],ux]
								endif
								if dum[-1] lt nux-1 then begin
									ext = sxus[dum[-1]:dum[-1]+1]
									if (xran[1]-ext[0]) ge 0.5*(ext[1]-ext[0]) $
									then ux = [ux,xran[1]]
								endif
							endif
							wsim[1] = n_elements(ux)
						endif else message, 'Bad XUSE input!'
						chfl = 0
					end
		endcase
		if not Isnum(ux) then begin
			rx = Make_grid(xtem[ind],wsim[1])
			if Sign(wx[sim[1]-1,sim[2]-1] - wx[0,0]) lt 0 then rx = reverse(rx)
		endif else rx = ux

		case One_of(ysm,yus) of
			-1	:
			0	:	begin
						dwy = Fltround(min(abs(Dif(wy[0,*],/lin))),dig=3)
						wsim[2] = round((max(yran,min=min) - min)/dwy) + 1
						chfl = 0
					end
			1	:	begin
						syus = yus[Sorpurge(yus,net=nuy)]
						dum = where(syus ge yran[0] and syus le yran[1],ndum)
						if ndum gt 0 then begin
							uy = syus[dum]
							if not keyword_set(str) then begin
								if dum[0] gt 0 then begin
									ext = syus[dum[0]-1:dum[0]]
									if (ext[1]-yran[0]) ge 0.5*(ext[1]-ext[0]) $
									then uy = [yran[0],uy]
								endif
								if dum[-1] lt nuy-1 then begin
									ext = syus[dum[-1]:dum[-1]+1]
									if (yran[1]-ext[0]) ge 0.5*(ext[1]-ext[0]) $
									then uy = [uy,yran[1]]
								endif
							endif
							wsim[2] = n_elements(uy)
						endif else message, 'Bad XUSE input!'
						chfl = 0
					end
		endcase
		if not Isnum(uy) then begin
			ry = Make_grid(ytem[ind],wsim[2])
			if Sign(wy[sim[1]-1,sim[2]-1] - wy[0,0]) lt 0 then ry = reverse(ry)
		endif else ry = uy

		if chfl then begin
			drx = min(Dif(rx,/lin))
			dry = min(Dif(ry,/lin))
			checkx = max(abs(rx#replicate(1,sim[2]) - wx))
			checky = max(abs(replicate(1,sim[1])#ry - wy))
			if checkx lt drx/2 and checky lt dry/2 then tring = 0 $
			else triangulate, wx, wy, tring
		endif else triangulate, wx, wy, tring
		exs = 1
	endif else if not Arreq([(size(wx))[1],(size(wy))[2]],sim[1:2]) then $
	message, 'Size mismatch!'

	rtx = rx
	rty = ry

	if Arreq(tring,0) then return, wimg $
	else return, FPU_fix(trigrid(wx,wy,wimg,tring,xout=rx,yout=ry)) > 0
end