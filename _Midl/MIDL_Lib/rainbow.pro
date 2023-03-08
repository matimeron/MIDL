Pro Rainbow, show = sho, inset = ins, number = num, location = loc, erase = era

;+
; NAME:
;		RAINBOW
; VERSION:
;		8.475
; PURPOSE:
;		Creates a system variable named !RAINBOW which contains an array of true
;		color values, for plotting purposes.
; CATEGORY:
;		Utility.
; CALLING SEQUENCE:
;		RAINBOW
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/SHOW
;		Switch.  If set, a plot using lines with all the defined colors is
;		generated.
;	/INSET
;		Switch.  If set, a plot as in /SHOW is generated as an inset within
;		current plot.
;	NUMBER
;		Integer scalar, the number of colors to display when SHOW is set.
;		Default is all the colors in !RAINBOW.
;	LOCATION
;		Character input, specifies location of inset.  Four acceptable inputs:
;			UR	:	Upper Right corner.
;			UL	:	Upper Left corner.
;			LL	:	Lower Left corner.
;			LR	:	Lower Right corner.
;
;		Default location is UR.
;		If /INSET is not set, LOCATION has no effect.
;	/ERASE
;		Switch.  If set when INSET is set, erases the inset.  Note, for the
;		erasure to work then value of NUMBER, if provided, must be the value
;		that was used plotting the inset.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None
; PROCEDURE:
;		If !RAINBOW doesn't exist, it is created, else a listing of the colors 
;		is displayed (unless /INSET is set).  The colors present in !RAINBOW 
;		are, in order:
;		
;			BLACK
;			RED
;			ORANGE
;			GREEN
;			BLUE-GREEN
;			BLUE
;			PURPLE
;			GREY
;		
;		RAINBOW calls LABELS, STRMACH_MM, TABULATE and TRUCOL from MIDL.
; MODIFICATION HISTORY:
;		Created 20-APR-2010 by Mati Meron, as a simplified version of PCOLS.
;		Modified 25-JUL-2011 by Mati Meron.  Added the color "yellow".  Added
;		keyword SHOW.
;		Modified 5-OCT-2011 by Mati Meron.  Removed color "yellow".
;		Modified 15-JUN-2015 by Mati Meron.  Added keywords INSET and LOCATION.
;		Modified 1-SEP-2016 by Mati Meron.  Added keywords NUMBER and ERASE.
;-

	rgb4 = [[0,0,0],[4,1,1],[4,3,0],[1,4,1],[0,4,4],[1,1,4],[3,0,3],[2,2,2]]
	cnams = ['black', 'red', 'orange', 'green', 'cyan','blue','purple','grey']

	infl = keyword_set(ins)
	defsysv, '!rainbow', exists = exs
	if exs and not infl then Tabulate, cnams, /ind, head = ['!Rainbow colors'] $
	else defsysv, '!rainbow', Trucol(rgb4), 1

	if keyword_set(sho) or infl then begin
		if infl then begin
			posib = ['ur','ul','ll','lr']
			rar = [[.7,.7,1,1],[.07,.7,.37,1],[.07,.05,.37,.35],[.7,.05,1.,.35]]
			wloc = 	Strmatch_mm(loc,posib,2) > 0
			wreg = rar[*,wloc]
			tem = !p.region
			!p.region = wreg
		endif else window, 0

		ncol = (ncolr = n_elements(!rainbow))
		if keyword_set(era) and infl then begin
			pcol = replicate(!pcol.white,ncol)
			lcol = !p.background
		endif else begin
			pcol = !rainbow
			lcol = !p.color
		endelse
		if keyword_set(num) then ncol = ncol < num

		sca = 1.8 - infl
		hshf = 1 + 2*infl
		vshf = 1 + infl*(3.*ncol/ncolr - 1)

		plot, findgen(2), xran = [0,1], yran = [0,ncol-1], /nodata,$
		xmargin = [8,6], ymargin = [4,4], xstyle = 5, ystyle = 5, noerase= infl
		for i = 0, ncol-1 do plots, [0,1],[i,i], col=pcol[i], thi=4*sca
		Labels, -0.01*hshf, findgen(ncol)-0.1*vshf, string(indgen(ncol), $
		form='(i0)'), charsize = sca, charthick = sca, col = lcol, align = 1
		if infl then !p.region = tem
	endif

	return
end