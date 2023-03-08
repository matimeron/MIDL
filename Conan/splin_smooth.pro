Function Splin_smooth, x, y, width = wid, minima = min, maxima = max, $
	threshold = tre, half_step = has, full_step = fus, single_pass = sip, $
	global = glo, x_ret = wx

	on_error, 1

	n = Split_xy(x,y,x=wx,y=wy,mint=4)
	mode = 1 + One_of(min,max)
	step = 1 + One_of(has,fus)
	if step eq 0 then if mode eq 0 then step = 1 else step = 2
	sifl = keyword_set(sip) or (mode eq 0 and step eq 2)
	tre = Default(tre,2*Toler(wy),/dtyp)

	if keyword_set(glo) then begin
		repeat begin
			case mode of
				0	:	ext = Extrema(wy,thre=tre,num=nex)
				1	:	ext = Extrema(wy,thre=tre,num=nex,/min)
				2	:	ext = Extrema(wy,thre=tre,num=nex,/max)
			endcase
			if nex gt 1 then begin
				if step eq 1 then begin
					tx = [wx[ext] + wx[ext-1],wx[ext] + wx[ext+1]]/2
					ty = [wy[ext] + wy[ext-1],wy[ext] + wy[ext+1]]/2
				endif else begin
					tx = wx[[ext-1,ext+1]]
					ty = wy[[ext-1,ext+1]]
				endelse
				spl = Splin_coeffs(tx,ty)
				wy[ext[0]:ext[nex-1]] = Splin_eval(wx[ext[0]:ext[nex-1]],spl)
			endif
		endrep until nex le 1 or sifl
	endif else begin
		wid = Default(wid,1+(mode gt 0),/dtyp)
		case mode of
			0	:	ext = Extrema(wy,thre=tre,num=nex)
			1	:	ext = Extrema(wy,thre=tre,num=nex,/min)
			2	:	ext = Extrema(wy,thre=tre,num=nex,/max)
		endcase
		fir = 0l
		las = 0l
		for i =	1l, nex-1 do begin
			if ext[i] - ext[i-1] le wid then begin
				if i eq (nex-1) then las = ext[i]
				if fir eq 0 then fir = ext[i-1]
			endif else if fir gt 0 then las = ext[i-1]
			if las gt 0 then begin
				wy[fir-1:las+1]= Splin_smooth(wx[fir-1:las+1],wy[fir-1:las+1],$
				/glob,min=min,max=max,thre= tre,half= has,full= fus, sing= sip)
				fir = 0l
				las = 0l
			endif
		endfor
	endelse

	return, wy
end