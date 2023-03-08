Function UN_struct, npo_max = nmx, nhar_max = nhmx

    common un_consts, hcove, alpha, oovee, ecf

    hcove = 1.2398424e-06
    alpha = 7.2973530e-3
    oovee = 6.2415064e+18
    ecf   = 1e-3

    nmx = 255
    nhmx = 512
    tem = fltarr(2*nmx+1)
    u = {unharm, name: string('undulator',format='(a32)'), k: 0., lamb: 0., $
	gamm: 0., nper: 1l, nh: 0l, harms: lonarr(nhmx), nxy: [0l,0l], $
	ganx: tem, gany: tem, conv: 0, rsig: [0.,0.], asig: [0.,0.]}

    return, u
end
