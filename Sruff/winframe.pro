Function Winframe, x, y, window = win, xygrid = wtem, $
    spacing = dxy, windex = qxy, fringe = fri

    on_error, 1

    sx = size(x)
    sy = size(y)
    if Arreq([sx[0],sy[0]],[1,1]) then begin
	wx = x
	wy = y
    endif else begin
	if Arreq([sx[0],sy[0]],[2,2]) and Arreq(sx,sy) then begin
	    wx = reform(x[*,0])
	    wy = reform(y[0,*])
	endif else message, 'improper or mismatched dimensions!'
    endelse

    sxy = [n_elements(wx),n_elements(wy)]
    dxy = abs([wx[sxy[0]-1] - wx[0], wy[sxy[1]-1] - wy[0]])/(sxy - 1.)
    dwin = sxy*dxy/2
    mwin = (sxy*(1-Toler()) + 1)*dxy/2
    case n_elements(win) of
	0    :	win = dwin
	1    :	win = [win,win] < mwin
	2    :	win = win < mwin
	else :	message, 'Window cannot have more then 2 elements!'
    endcase

    wtem = Make_grid([[wx[[0,sxy[0]-1]]],[wy[[0,sxy[1]-1]]]],sxy,fun=frame)
    qxy = floor(win/dxy)
    fri = win/dxy - qxy - 0.5*(sxy mod 2)
    lo = (sxy - 2*qxy)/2
    hi = (sxy - 1 + 2*qxy)/2

    frame(lo[0]:hi[0],lo[1]:hi[1]) = 1

    return, frame
end
