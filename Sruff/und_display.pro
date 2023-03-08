Pro Und_display, dat, x, y, ind= ind, image= img, contour= cnt, surface= sur, $
	title = tit, subtitle = sti, xtitle = xti, ytitle = yti, scale_down = scd,$
	wuse = wus, wnew = new, _extra = _e

	on_error, 1

	cwin = !d.window
	if Isnum(wus) then dwin = wus $
	else dwin = ((cwin + keyword_set(new)) > 0) mod 32
	siz = size(dat)
	case siz[0] of
		2	:	dim = siz[1:2]
		3	:	dim = siz[2:3]
		else:	message, 'Bad input!'
	endcase
	wscd = Default(scd,1,/dtyp)
	zom = (512/(wscd*max(dim)))/2*2 + 1
	Display_mm, dat, x, y, zoom=zom, wsi=wsi, /nodata
	case (One_of(img,cnt,sur,/noz) > 0) of
		0	:	wsi = wsi + [0,12]
		1	:	begin
					wsi = wsi + [-32,0]
					xmar = [10,11]
					ymar = [6,4]
				end
		2	:	begin
					wsi = wsi + [64,0]
					xmar=[8,8]
				end
	endcase
	window, dwin, xsiz = wsi[0], ysiz = wsi[1]

	Display_mm, dat, x, y, ind=ind, image= img, contour= cnt, surface= sur, $
	title= tit, subtitle= sti, xtitle= xti, ytitle= yti, zoom= zom, $
	xmar= xmar, ymar= ymar, _extra = _e

	return
end