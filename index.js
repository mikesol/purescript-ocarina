var Dg=function(t){return function(r){for(var e=r.length,n=new Array(e),u=0;u<e;u++)n[u]=t(r[u]);return n}};var gi={compose:function(t){return function(r){return function(e){return t(r(e))}}}},fa=function(t){return t.compose},ip=function(t){var r=fa(t);return function(e){return function(n){return r(n)(e)}}};var K=function(t){return t.identity},Z={identity:function(t){return t},Semigroupoid0:function(){return gi}};var Jr=!0;var Jt=function(t){return function(r){return function(e){return t(e)(r)}}},w=function(t){return function(r){return t}};var Jf=function(t){return function(r){return r(t)}},Mc=function(t){return function(r){return t(r)}};var d=function(){function t(){}return t.value=new t,t}();var S=function(t){return t.map},oe=function(t){var r=S(t);return function(e){return function(n){return r(n)(e)}}},Pr=function(t){return S(t)(w(void 0))},W=function(t){var r=S(t);return function(e){return function(n){return r(w(n))(e)}}},xv=function(t){var r=S(t);return function(e){return r(w(e))}};var Qe={map:fa(gi)},ir={map:Dg},CD=function(t){var r=S(t);return function(e){return function(n){return r(function(u){return u(n)})(e)}}};var bg=function(t){return function(r){return t+r}},Ag=function(t){return function(r){return t.length===0?r:r.length===0?t:t.concat(r)}};var fe=function(t){return t.reflectSymbol};var Jo=function(t){var r=function(e){var n;function u(a){e=a}for(;;)n=u(e);return n};return r(t)};var Cv=function(t){return function(r){return{}.hasOwnProperty.call(r,t)}},Au=function(t){return function(r){return r[t]}},qa=function(t){return function(r){return function(e){var n={};for(var u in e)({}).hasOwnProperty.call(e,u)&&(n[u]=e[u]);return n[t]=r,n}}};var kg={append:function(t){return function(r){return void 0}}},$v={append:bg};var He={append:Ag};var tt=function(t){return t.append},$D=function(t){var r=tt(t);return{append:function(e){return function(n){return function(u){return r(e(u))(n(u))}}}}};var ht=function(t){return t.alt};var dg=function(t){return function(r){for(var e=t.length,n=r.length,u=new Array(e*n),a=0,o=0;o<e;o++)for(var i=t[o],f=0;f<n;f++)u[a++]=i(r[f]);return u}};var Pw=K(Z);var fp={apply:dg,Functor0:function(){return ir}},it=function(t){return t.apply};var rr=function(t){var r=it(t),e=S(t.Functor0());return function(n){return function(u){return r(e(w(Pw))(n))(u)}}},$e=function(t){var r=it(t),e=S(t.Functor0());return function(n){return function(u){return function(a){return r(e(n)(u))(a)}}}};var M=function(t){return t.pure};var Pn=function(t){var r=M(t);return function(e){return function(n){if(e)return n;if(!e)return r(void 0);throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): "+[e.constructor.name,n.constructor.name])}}},cp=function(t){var r=it(t.Apply0()),e=M(t);return function(n){return function(u){return r(e(n))(u)}}};var Ha={pure:function(t){return[t]},Apply0:function(){return fp}};var yg=function(t){return function(r){for(var e=[],n=0,u=t.length;n<u;n++)Array.prototype.push.apply(e,r(t[n]));return e}};var Lw=K(Z);var hi={bind:yg,Apply0:function(){return fp}},lt=function(t){return t.bind},cn=function(t){return Jt(lt(t))};var Oc=function(t){var r=lt(t);return function(e){return function(n){return function(u){return r(e(u))(n)}}}};var qu=function(t){var r=lt(t);return function(e){return r(e)(Lw)}};var Ke=function(t){return function(r){for(var e=t>r?-1:1,n=new Array(e*(r-t)+1),u=t,a=0;u!==r;)n[a++]=u,u+=e;return n[a]=u,n}},Nw=function(t){return function(r){if(t<1)return[];var e=new Array(t);return e.fill(r)}},Uw=function(t){return function(r){for(var e=[],n=0,u=0;u<t;u++)e[n++]=r;return e}},Tv=typeof Array.prototype.fill=="function"?Nw:Uw,Bw=function(){function t(u,a){this.head=u,this.tail=a}var r={};function e(u){return function(a){return new t(u,a)}}function n(u){for(var a=[],o=0,i=u;i!==r;)a[o++]=i.head,i=i.tail;return a}return function(u){return function(a){return n(u(e)(r)(a))}}}(),Vn=function(t){return t.length};var gg=function(t){return function(r){return function(e){return function(n){return n<0||n>=e.length?r:t(e[n])}}}};var hg=function(t){return function(r){return function(e){return function(n){for(var u=0,a=n.length;u<a;u++)if(e(n[u]))return t(u);return r}}}};var Sg=function(t){return function(r){return function(e){return function(n){if(e<0||e>=n.length)return r;var u=n.slice();return u.splice(e,1),t(u)}}}};var Ww=function(){function t(r,e,n,u,a,o){var i,f,m,v,D,A,b;for(i=a+(o-a>>1),i-a>1&&t(r,e,u,n,a,i),o-i>1&&t(r,e,u,n,i,o),f=a,m=i,v=a;f<i&&m<o;)D=u[f],A=u[m],b=e(r(D)(A)),b>0?(n[v++]=A,++m):(n[v++]=D,++f);for(;f<i;)n[v++]=u[f++];for(;m<o;)n[v++]=u[m++]}return function(r){return function(e){return function(n){var u;return n.length<2?n:(u=n.slice(0),t(r,e,u,n.slice(0),0,n.length),u)}}}}();var wc=function(t){return function(r){return function(e){for(var n=r.length<e.length?r.length:e.length,u=new Array(n),a=0;a<n;a++)u[a]=t(r[a])(e[a]);return u}}};var xg=function(t){return function(r){return t[r]}};var Hw=function(t,r,e){var n=0,u;return function(a){if(n===2)return u;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+a+")",r,a);return n=1,u=e(),n=2,u}};var Cg={defer:function(t){return function(r){return t(void 0)(r)}}},Pc=function(t){return t.defer},ED=function(t){var r=Pc(t);return function(e){var n=Hw("go","Control.Lazy",function(){return r(function(a){return e(n(25))})}),u=n(25);return u}};var Gn=function(t){var r=lt(t.Bind1()),e=M(t.Applicative0());return function(n){return function(u){return r(n)(function(a){return r(u)(function(o){return e(a(o))})})}}};var zw=String.fromCharCode(65535),Vw=String.fromCharCode(0),Gw=Number.POSITIVE_INFINITY,Jw=Number.NEGATIVE_INFINITY;var $g=function(t){return function(r){return function(e){return function(n){return function(u){return n<u?t:n===u?r:e}}}}};var Tg=$g,Fg=$g;var Eg=function(t){return function(r){return t===r}};var Mg=Eg,Og=Eg;var lp={eq:Og},eo={eq:Mg};var yt=function(t){return t.eq};var It=function(){function t(){}return t.value=new t,t}(),jt=function(){function t(){}return t.value=new t,t}(),Qt=function(){function t(){}return t.value=new t,t}();var wg=function(t){return function(r){return t-r|0}},Pg=function(t){return function(r){return t-r}};var Ig=function(t){return function(r){return t+r|0}},Rg=function(t){return function(r){return t*r|0}},Lg=function(t){return function(r){return t+r}},Ng=function(t){return function(r){return t*r}};var gn=function(t){return t.zero};var Jn={add:Lg,zero:0,mul:Ng,one:1},pa={add:Ig,zero:0,mul:Rg,one:1};var In=function(t){return t.one};var Ye=function(t){return t.mul};var Er=function(t){return t.add};var du=function(t){return t.sub};var jf={sub:Pg,Semiring0:function(){return Jn}},MD={sub:wg,Semiring0:function(){return pa}};var pp=function(t){var r=du(t),e=gn(t.Semiring0());return function(n){return r(e)(n)}};var za=function(){return{compare:Fg(It.value)(Qt.value)(jt.value),Eq0:function(){return lp}}}(),Te=function(){return{compare:Tg(It.value)(Qt.value)(jt.value),Eq0:function(){return eo}}}();var Rt=function(t){return t.compare};var Hg=function(t){var r=Rt(t);return function(e){return function(n){var u=r(e)(n);return!(u instanceof It)}}};var Xo=function(t){var r=Rt(t);return function(e){return function(n){var u=r(e)(n);if(u instanceof It)return n;if(u instanceof Qt||u instanceof jt)return e;throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): "+[u.constructor.name])}}};var wD=function(t){var r=Hg(t);return function(e){var n=gn(e.Semiring0()),u=pp(e);return function(a){var o=r(a)(n);return o?a:u(a)}}};var Rn=function(t){return t.top};var vp={top:2147483647,bottom:-2147483648,Ord0:function(){return Te}};var Ln=function(t){return t.bottom};var Vg=function(t){return t.toString()};var gu={show:Vg};var Et=function(t){return t.show};var lP=K(Z),P=function(){function t(){}return t.value=new t,t}(),F=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var gt=function(t){return function(r){return function(e){if(e instanceof P)return t;if(e instanceof F)return r(e.value0);throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}};var Ae={map:function(t){return function(r){return r instanceof F?new F(t(r.value0)):P.value}}},pP=S(Ae);var Sn=function(t){return gt(t)(lP)},xn=function(){return function(t){if(t instanceof F)return t.value0;throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): "+[t.constructor.name])}};var Ko={apply:function(t){return function(r){if(t instanceof F)return pP(t.value0)(r);if(t instanceof P)return P.value;throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): "+[t.constructor.name,r.constructor.name])}},Functor0:function(){return Ae}};var Nn={bind:function(t){return function(r){if(t instanceof F)return r(t.value0);if(t instanceof P)return P.value;throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): "+[t.constructor.name,r.constructor.name])}},Apply0:function(){return Ko}};var no=function(){return{pure:F.create,Apply0:function(){return Ko}}}();var xt=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Ct=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var Qf={map:function(t){return function(r){if(r instanceof xt)return new xt(r.value0);if(r instanceof Ct)return new Ct(t(r.value0));throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): "+[r.constructor.name])}}};var jn=function(t){return function(r){return function(e){if(e instanceof xt)return t(e.value0);if(e instanceof Ct)return r(e.value0);throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},Fv=function(){return jn(w(P.value))(F.create)}();var Hu=function(t){return t};var Va={map:function(t){return function(r){return t(r)}}};var Gg={apply:function(t){return function(r){return t(r)}},Functor0:function(){return Va}},_P={bind:function(t){return function(r){return r(t)}},Apply0:function(){return Gg}},ID={pure:Hu,Apply0:function(){return Gg}},_a={Applicative0:function(){return ID},Bind1:function(){return _P}};var Jg=function(t){return Math.min(Math.abs(t),2147483647)},jg=function(t){return function(r){return r===0?0:r>0?Math.floor(t/r):-Math.floor(t/-r)}},Xg=function(t){return function(r){if(r===0)return 0;var e=Math.abs(r);return(t%e+e)%e}},Qg=function(t){return function(r){return t/r}};var Kg={Ring0:function(){return jf}},Yg={Ring0:function(){return MD}};var hu=function(t){return t.mod};var Ev={degree:function(t){return 1},div:Qg,mod:function(t){return function(r){return 0}},CommutativeRing0:function(){return Kg}},Ga={degree:Jg,div:jg,mod:Xg,CommutativeRing0:function(){return Yg}},Ja=function(t){return t.div};var uo={mempty:void 0,Semigroup0:function(){return kg}};var Ut=function(t){return t.mempty},Kf=function(t){var r=Ut(t),e=$D(t.Semigroup0());return{mempty:function(n){return r},Semigroup0:function(){return e}}};var RD=function(t){return function(){return t}},Zg=function(t){return function(r){return function(){return r(t())()}}};var bp=function(t){return function(r){return function(){for(var e=0,n=t.length;e<n;e++)r(t[e])()}}};var th=function(t,r,e){var n=0,u;return function(a){if(n===2)return u;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+a+")",r,a);return n=1,u=e(),n=2,u}},ao={Applicative0:function(){return Wt},Bind1:function(){return Qn}},Qn={bind:Zg,Apply0:function(){return LD(0)}},Wt={pure:RD,Apply0:function(){return LD(0)}},rh=th("functorEffect","Effect",function(){return{map:cp(Wt)}}),LD=th("applyEffect","Effect",function(){return{apply:Gn(ao),Functor0:function(){return rh(0)}}}),L=rh(20),or=LD(23),DP=$e(or),ND=function(t){return{append:DP(tt(t))}},io=function(t){var r=ND(t.Semigroup0());return{mempty:RD(Ut(t)),Semigroup0:function(){return r}}};var eh=function(t){return function(){return{value:t}}};var Ur=function(t){return function(){return t.value}},nh=function(t){return function(r){return function(){var e=t(r.value);return r.value=e.state,e.value}}},te=function(t){return function(r){return function(){r.value=t}}};var AP=Pr(L),Or=eh,kP=nh,Yf=function(t){return kP(function(r){var e=t(r);return{state:e,value:e}})},oo=function(t){return function(r){return AP(Yf(t)(r))}};var uh=function(t){return function(r){return function(){return t(r())}}},ah=function(t){return function(){return t}},ih=function(t){return function(r){return function(){return r(t())()}}};function Fe(t){return function(){return{value:t}}}var Ze=function(t){return function(){return t.value}},oh=function(t){return function(r){return function(){var e=t(r.value);return r.value=e.state,e.value}}},Cu=function(t){return function(r){return function(){return r.value=t}}};var xP=function(t,r,e){var n=0,u;return function(a){if(n===2)return u;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+a+")",r,a);return n=1,u=e(),n=2,u}},CP=oh,ja=function(t){return CP(function(r){var e=t(r);return{state:e,value:e}})},Kn={map:uh};var BD={Applicative0:function(){return Ee},Bind1:function(){return Nc}},Nc={bind:ih,Apply0:function(){return WD(0)}},Ee={pure:ah,Apply0:function(){return WD(0)}},WD=xP("applyST","Control.Monad.ST.Internal",function(){return{apply:Gn(BD),Functor0:function(){return Kn}}}),Mv=WD(47),$P=$e(Mv);var TP=M(Ee),qD=function(t){return{append:$P(tt(t))}};var ch=function(t){var r=qD(t.Semigroup0());return{mempty:TP(Ut(t)),Semigroup0:function(){return r}}};function Yo(){return[]}var HD=function(t){return function(r){return function(){return r.push.apply(r,t)}}};var Ov=function(t){return function(r){return function(e){return function(n){return function(){return n.splice.apply(n,[t,r].concat(e))}}}}};function FP(t){return function(){return t.slice()}}var dp=FP;var EP=function(){function t(r,e,n,u,a,o){var i,f,m,v,D,A,b;for(i=a+(o-a>>1),i-a>1&&t(r,e,u,n,a,i),o-i>1&&t(r,e,u,n,i,o),f=a,m=i,v=a;f<i&&m<o;)D=u[f],A=u[m],b=e(r(D)(A)),b>0?(n[v++]=A,++m):(n[v++]=D,++f);for(;f<i;)n[v++]=u[f++];for(;m<o;)n[v++]=u[m++]}return function(r){return function(e){return function(n){return function(){return n.length<2||t(r,e,n,n.slice(0),0,n.length),n}}}}}();var co=function(t){return HD([t])};var sh=function(t){return function(r){return function(e){for(var n=r,u=e.length,a=u-1;a>=0;a--)n=t(e[a])(n);return n}}},mh=function(t){return function(r){return function(e){for(var n=r,u=e.length,a=0;a<u;a++)n=t(n)(e[a]);return n}}};var x=function(t){return t.empty};var U=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),qc=function(t){return function(r){return t(r.value0)(r.value1)}};var Ne=function(t){return t.value1};var lo={map:function(t){return function(r){return new U(r.value0,t(r.value1))}}};var pu=function(t){return t.value0};var Zf=function(t){return function(r){return function(e){return t(new U(r,e))}}};var j=function(t){return t};var tn=function(){return j};var jD=tn(),po=function(){return jD};var ie=function(){return jD};var kh=function(){return function(){return function(t){return jD}}};var Xr=function(t){return t.foldr};var rn=function(t){var r=Xr(t);return function(e){return r(ht(e.Alt0()))(x(e))}},_u=function(t){var r=Xr(t);return function(e){var n=ht(e.Alt0()),u=x(e);return function(a){return r(function(o){return n(a(o))})(u)}}},Dn=function(t){var r=rr(t.Apply0()),e=M(t);return function(n){var u=Xr(n);return function(a){return u(function(o){return r(a(o))})(e(void 0))}}},$u=function(t){var r=Dn(t);return function(e){return Jt(r(e))}};var Qr=function(t){return t.foldl};var Me={foldr:function(t){return function(r){return function(e){if(e instanceof P)return r;if(e instanceof F)return t(e.value0)(r);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},foldl:function(t){return function(r){return function(e){if(e instanceof P)return r;if(e instanceof F)return t(r)(e.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},foldMap:function(t){var r=Ut(t);return function(e){return function(n){if(n instanceof P)return r;if(n instanceof F)return e(n.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[e.constructor.name,n.constructor.name])}}}};var yh=function(t){var r=Xr(t);return function(e){var n=tt(e.Semigroup0()),u=Ut(e);return function(a){return r(function(o){return function(i){return n(a(o))(i)}})(u)}}},Ht={foldr:sh,foldl:mh,foldMap:function(t){return yh(Ht)(t)}};var ge=function(t){return t.foldMap};var gh=function(t){return function(r){for(var e=r.length,n=Array(e),u=0;u<e;u++)n[u]=t(u)(r[u]);return n}};var Tu=function(t){return t.mapWithIndex};var Da={mapWithIndex:gh,Functor0:function(){return ir}};var xh=function(){function t(u){return[u]}function r(u){return function(a){return[u,a]}}function e(u){return function(a){return function(o){return[u,a,o]}}}function n(u){return function(a){return u.concat(a)}}return function(u){return function(a){return function(o){return function(i){return function(f){function m(v,D){switch(D-v){case 0:return o([]);case 1:return a(t)(i(f[v]));case 2:return u(a(r)(i(f[v])))(i(f[v+1]));case 3:return u(u(a(e)(i(f[v])))(i(f[v+1])))(i(f[v+2]));default:var A=v+Math.floor((D-v)/4)*2;return u(a(n)(m(v,A)))(m(A,D))}}return m(0,f.length)}}}}}}();var F1=K(Z),en=function(t){return t.traverse};var Oh=function(t){var r=en(t);return function(e){return r(e)(F1)}},xi={traverse:function(t){var r=t.Apply0();return xh(it(r))(S(r.Functor0()))(M(t))},sequence:function(t){return Oh(xi)(t)},Functor0:function(){return ir},Foldable1:function(){return Ht}};var G1=xn();var J1=tt(He);var Rp=function(){return wc(U.create)}();var zc=function(){return xg};var Lh=function(t){return[t]};var Vc=function(){return gg(F.create)(P.value)}(),lb=function(t){return Vc(t)(Vn(t)-1|0)};var Nh=function(){return hg(F.create)(P.value)}();var pb=function(){return Sg(F.create)(P.value)}(),_b=function(t){return function(r){return function(e){return e.length===0?[]:gt(e)(function(n){return G1(pb(n)(e))})(Nh(t(r))(e))}}};var tf=function(t){return function(r){return J1([t])(r)}};var $i=function(t){return t.foldrWithIndex};var Xa=function(t){return t.foldlWithIndex};var vo=function(t){return t.foldMapWithIndex};var rf=function(t){return t.traverseWithIndex};var Ka=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}();var Hv=function(t){var r=x(t);return function(e){return new Ka(e,r)}};var wr=function(){function t(){}return t.value=new t,t}(),Mt=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),zv=function(t){return t},aI=function(t){return new Mt(t.value0,t.value1)};var iI=function(t){var r=function(e){return function(n){var u=e,a=!1,o;function i(f,m){if(m instanceof Mt&&m.value1 instanceof Mt&&m.value1.value1 instanceof Mt){u=new Mt(m,f),n=m.value1.value1.value1;return}var v=function(A){return A instanceof Mt&&A.value1 instanceof Mt&&A.value1.value1 instanceof wr?new Mt(t(A.value0),new Mt(t(A.value1.value0),wr.value)):A instanceof Mt&&A.value1 instanceof wr?new Mt(t(A.value0),wr.value):wr.value},D=function(A){return function(b){var _=A,k=!1,G;function nt(bt,vr){if(bt instanceof Mt&&bt.value0 instanceof Mt&&bt.value0.value1 instanceof Mt&&bt.value0.value1.value1 instanceof Mt){_=bt.value1,b=new Mt(t(bt.value0.value0),new Mt(t(bt.value0.value1.value0),new Mt(t(bt.value0.value1.value1.value0),vr)));return}return k=!0,vr}for(;!k;)G=nt(_,b);return G}};return a=!0,D(f)(v(m))}for(;!a;)o=i(u,n);return o}};return r(wr.value)},Lp={map:iI};var vu={foldr:function(t){return function(r){var e=function(){var u=function(a){return function(o){var i=a,f=!1,m;function v(D,A){if(A instanceof wr)return f=!0,D;if(A instanceof Mt){i=new Mt(A.value0,D),o=A.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): "+[D.constructor.name,A.constructor.name])}for(;!f;)m=v(i,o);return m}};return u(wr.value)}(),n=Qr(vu)(Jt(t))(r);return function(u){return n(e(u))}}},foldl:function(t){var r=function(e){return function(n){var u=e,a=!1,o;function i(f,m){if(m instanceof wr)return a=!0,f;if(m instanceof Mt){u=t(f)(m.value0),n=m.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): "+[m.constructor.name])}for(;!a;)o=i(u,n);return o}};return r},foldMap:function(t){var r=tt(t.Semigroup0()),e=Ut(t);return function(n){return Qr(vu)(function(u){var a=r(u);return function(o){return a(n(o))}})(e)}}};var oI=Xr(vu);var Vv={append:function(t){return function(r){return oI(Mt.create)(r)(t)}}},Wh=tt(Vv);var mb={append:function(t){return function(r){return new Ka(t.value0,Wh(t.value1)(aI(r)))}}};var qh={alt:Wh,Functor0:function(){return Lp}},Db=function(){return{empty:wr.value,Alt0:function(){return qh}}}();var bb=function(){var t=function(r){return function(e){var n=r,u=!1,a;function o(i,f){if(f instanceof wr)return u=!0,i;if(f instanceof Mt){n=new Mt(f.value0,i),e=f.value1;return}throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): "+[i.constructor.name,f.constructor.name])}for(;!u;)a=o(n,e);return a}};return t(wr.value)}();var jh=function(t){return t()};var Xh=function(t){throw new Error(t)};var Qh=function(){return Xh};var FI=Qh(),EI=jh,Fu=function(t){return EI(function(){return FI(t)})};var Lt=function(){function t(){}return t.value=new t,t}(),er=function(){function t(r,e,n,u){this.value0=r,this.value1=e,this.value2=n,this.value3=u}return t.create=function(r){return function(e){return function(n){return function(u){return new t(r,e,n,u)}}}},t}(),Ir=function(){function t(r,e,n,u,a,o,i){this.value0=r,this.value1=e,this.value2=n,this.value3=u,this.value4=a,this.value5=o,this.value6=i}return t.create=function(r){return function(e){return function(n){return function(u){return function(a){return function(o){return function(i){return new t(r,e,n,u,a,o,i)}}}}}}},t}(),uf=function(){function t(r,e,n){this.value0=r,this.value1=e,this.value2=n}return t.create=function(r){return function(e){return function(n){return new t(r,e,n)}}},t}(),mo=function(){function t(r,e,n){this.value0=r,this.value1=e,this.value2=n}return t.create=function(r){return function(e){return function(n){return new t(r,e,n)}}},t}(),af=function(){function t(r,e,n,u,a,o){this.value0=r,this.value1=e,this.value2=n,this.value3=u,this.value4=a,this.value5=o}return t.create=function(r){return function(e){return function(n){return function(u){return function(a){return function(o){return new t(r,e,n,u,a,o)}}}}}},t}(),Ei=function(){function t(r,e,n,u,a,o){this.value0=r,this.value1=e,this.value2=n,this.value3=u,this.value4=a,this.value5=o}return t.create=function(r){return function(e){return function(n){return function(u){return function(a){return function(o){return new t(r,e,n,u,a,o)}}}}}},t}(),of=function(){function t(r,e,n,u,a,o){this.value0=r,this.value1=e,this.value2=n,this.value3=u,this.value4=a,this.value5=o}return t.create=function(r){return function(e){return function(n){return function(u){return function(a){return function(o){return new t(r,e,n,u,a,o)}}}}}},t}(),Jv=function(){function t(r,e,n,u){this.value0=r,this.value1=e,this.value2=n,this.value3=u}return t.create=function(r){return function(e){return function(n){return function(u){return new t(r,e,n,u)}}}},t}();var Zh=function(t){return function(r){return new er(Lt.value,t,r,Lt.value)}};var Xv=function(t){var r=Rt(t);return function(e){var n=function(u){var a=!1,o;function i(f){if(f instanceof Lt)return a=!0,P.value;if(f instanceof er){var m=r(e)(f.value1);if(m instanceof Qt)return a=!0,new F(f.value2);if(m instanceof It){u=f.value0;return}u=f.value3;return}if(f instanceof Ir){var v=r(e)(f.value1);if(v instanceof Qt)return a=!0,new F(f.value2);var D=r(e)(f.value4);if(D instanceof Qt)return a=!0,new F(f.value5);if(v instanceof It){u=f.value0;return}if(D instanceof jt){u=f.value6;return}u=f.value3;return}throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): "+[f.constructor.name])}for(;!a;)o=i(u);return o};return n}};var tS=function(t){return t instanceof Lt};var rS=function(t){return function(r){return function(e){var n=t,u=r,a=!1,o;function i(f,m,v){if(m instanceof wr)return a=!0,v;if(m instanceof Mt){if(m.value0 instanceof uf){n=f,u=m.value1,e=new er(v,m.value0.value0,m.value0.value1,m.value0.value2);return}if(m.value0 instanceof mo){n=f,u=m.value1,e=new er(m.value0.value0,m.value0.value1,m.value0.value2,v);return}if(m.value0 instanceof af){n=f,u=m.value1,e=new Ir(v,m.value0.value0,m.value0.value1,m.value0.value2,m.value0.value3,m.value0.value4,m.value0.value5);return}if(m.value0 instanceof Ei){n=f,u=m.value1,e=new Ir(m.value0.value0,m.value0.value1,m.value0.value2,v,m.value0.value3,m.value0.value4,m.value0.value5);return}if(m.value0 instanceof of){n=f,u=m.value1,e=new Ir(m.value0.value0,m.value0.value1,m.value0.value2,m.value0.value3,m.value0.value4,m.value0.value5,v);return}throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): "+[m.value0.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): "+[m.constructor.name,v.constructor.name])}for(;!a;)o=i(n,u,e);return o}}},Up=function(t){var r=rS(t),e=Rt(t);return function(n){return function(u){var a=function(i){return function(f){var m=i,v=!1,D;function A(b,_){if(b instanceof wr)return v=!0,new er(_.value0,_.value1,_.value2,_.value3);if(b instanceof Mt){if(b.value0 instanceof uf)return v=!0,r(b.value1)(new Ir(_.value0,_.value1,_.value2,_.value3,b.value0.value0,b.value0.value1,b.value0.value2));if(b.value0 instanceof mo)return v=!0,r(b.value1)(new Ir(b.value0.value0,b.value0.value1,b.value0.value2,_.value0,_.value1,_.value2,_.value3));if(b.value0 instanceof af){m=b.value1,f=new Jv(new er(_.value0,_.value1,_.value2,_.value3),b.value0.value0,b.value0.value1,new er(b.value0.value2,b.value0.value3,b.value0.value4,b.value0.value5));return}if(b.value0 instanceof Ei){m=b.value1,f=new Jv(new er(b.value0.value0,b.value0.value1,b.value0.value2,_.value0),_.value1,_.value2,new er(_.value3,b.value0.value3,b.value0.value4,b.value0.value5));return}if(b.value0 instanceof of){m=b.value1,f=new Jv(new er(b.value0.value0,b.value0.value1,b.value0.value2,b.value0.value3),b.value0.value4,b.value0.value5,new er(_.value0,_.value1,_.value2,_.value3));return}throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): "+[b.value0.constructor.name,_.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): "+[b.constructor.name,_.constructor.name])}for(;!v;)D=A(m,f);return D}},o=function(i){return function(f){var m=i,v=!1,D;function A(b,_){if(_ instanceof Lt)return v=!0,a(b)(new Jv(Lt.value,n,u,Lt.value));if(_ instanceof er){var k=e(n)(_.value1);if(k instanceof Qt)return v=!0,r(b)(new er(_.value0,n,u,_.value3));if(k instanceof It){m=new Mt(new uf(_.value1,_.value2,_.value3),b),f=_.value0;return}m=new Mt(new mo(_.value0,_.value1,_.value2),b),f=_.value3;return}if(_ instanceof Ir){var k=e(n)(_.value1);if(k instanceof Qt)return v=!0,r(b)(new Ir(_.value0,n,u,_.value3,_.value4,_.value5,_.value6));var G=e(n)(_.value4);if(G instanceof Qt)return v=!0,r(b)(new Ir(_.value0,_.value1,_.value2,_.value3,n,u,_.value6));if(k instanceof It){m=new Mt(new af(_.value1,_.value2,_.value3,_.value4,_.value5,_.value6),b),f=_.value0;return}if(k instanceof jt&&G instanceof It){m=new Mt(new Ei(_.value0,_.value1,_.value2,_.value4,_.value5,_.value6),b),f=_.value3;return}m=new Mt(new of(_.value0,_.value1,_.value2,_.value3,_.value4,_.value5),b),f=_.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): "+[b.constructor.name,_.constructor.name])}for(;!v;)D=A(m,f);return D}};return o(wr.value)}}},II=function(t){var r=rS(t),e=Rt(t);return function(n){var u=function(f){return function(m){var v=f,D=!1,A;function b(_,k){if(_ instanceof wr)return D=!0,k;if(_ instanceof Mt){if(_.value0 instanceof uf&&_.value0.value2 instanceof Lt&&k instanceof Lt)return D=!0,r(_.value1)(new er(Lt.value,_.value0.value0,_.value0.value1,Lt.value));if(_.value0 instanceof mo&&_.value0.value0 instanceof Lt&&k instanceof Lt)return D=!0,r(_.value1)(new er(Lt.value,_.value0.value1,_.value0.value2,Lt.value));if(_.value0 instanceof uf&&_.value0.value2 instanceof er){v=_.value1,m=new Ir(k,_.value0.value0,_.value0.value1,_.value0.value2.value0,_.value0.value2.value1,_.value0.value2.value2,_.value0.value2.value3);return}if(_.value0 instanceof mo&&_.value0.value0 instanceof er){v=_.value1,m=new Ir(_.value0.value0.value0,_.value0.value0.value1,_.value0.value0.value2,_.value0.value0.value3,_.value0.value1,_.value0.value2,k);return}return _.value0 instanceof uf&&_.value0.value2 instanceof Ir?(D=!0,r(_.value1)(new er(new er(k,_.value0.value0,_.value0.value1,_.value0.value2.value0),_.value0.value2.value1,_.value0.value2.value2,new er(_.value0.value2.value3,_.value0.value2.value4,_.value0.value2.value5,_.value0.value2.value6)))):_.value0 instanceof mo&&_.value0.value0 instanceof Ir?(D=!0,r(_.value1)(new er(new er(_.value0.value0.value0,_.value0.value0.value1,_.value0.value0.value2,_.value0.value0.value3),_.value0.value0.value4,_.value0.value0.value5,new er(_.value0.value0.value6,_.value0.value1,_.value0.value2,k)))):_.value0 instanceof af&&_.value0.value2 instanceof Lt&&_.value0.value5 instanceof Lt&&k instanceof Lt?(D=!0,r(_.value1)(new Ir(Lt.value,_.value0.value0,_.value0.value1,Lt.value,_.value0.value3,_.value0.value4,Lt.value))):_.value0 instanceof Ei&&_.value0.value0 instanceof Lt&&_.value0.value5 instanceof Lt&&k instanceof Lt?(D=!0,r(_.value1)(new Ir(Lt.value,_.value0.value1,_.value0.value2,Lt.value,_.value0.value3,_.value0.value4,Lt.value))):_.value0 instanceof of&&_.value0.value0 instanceof Lt&&_.value0.value3 instanceof Lt&&k instanceof Lt?(D=!0,r(_.value1)(new Ir(Lt.value,_.value0.value1,_.value0.value2,Lt.value,_.value0.value4,_.value0.value5,Lt.value))):_.value0 instanceof af&&_.value0.value2 instanceof er?(D=!0,r(_.value1)(new er(new Ir(k,_.value0.value0,_.value0.value1,_.value0.value2.value0,_.value0.value2.value1,_.value0.value2.value2,_.value0.value2.value3),_.value0.value3,_.value0.value4,_.value0.value5))):_.value0 instanceof Ei&&_.value0.value0 instanceof er?(D=!0,r(_.value1)(new er(new Ir(_.value0.value0.value0,_.value0.value0.value1,_.value0.value0.value2,_.value0.value0.value3,_.value0.value1,_.value0.value2,k),_.value0.value3,_.value0.value4,_.value0.value5))):_.value0 instanceof Ei&&_.value0.value5 instanceof er?(D=!0,r(_.value1)(new er(_.value0.value0,_.value0.value1,_.value0.value2,new Ir(k,_.value0.value3,_.value0.value4,_.value0.value5.value0,_.value0.value5.value1,_.value0.value5.value2,_.value0.value5.value3)))):_.value0 instanceof of&&_.value0.value3 instanceof er?(D=!0,r(_.value1)(new er(_.value0.value0,_.value0.value1,_.value0.value2,new Ir(_.value0.value3.value0,_.value0.value3.value1,_.value0.value3.value2,_.value0.value3.value3,_.value0.value4,_.value0.value5,k)))):_.value0 instanceof af&&_.value0.value2 instanceof Ir?(D=!0,r(_.value1)(new Ir(new er(k,_.value0.value0,_.value0.value1,_.value0.value2.value0),_.value0.value2.value1,_.value0.value2.value2,new er(_.value0.value2.value3,_.value0.value2.value4,_.value0.value2.value5,_.value0.value2.value6),_.value0.value3,_.value0.value4,_.value0.value5))):_.value0 instanceof Ei&&_.value0.value0 instanceof Ir?(D=!0,r(_.value1)(new Ir(new er(_.value0.value0.value0,_.value0.value0.value1,_.value0.value0.value2,_.value0.value0.value3),_.value0.value0.value4,_.value0.value0.value5,new er(_.value0.value0.value6,_.value0.value1,_.value0.value2,k),_.value0.value3,_.value0.value4,_.value0.value5))):_.value0 instanceof Ei&&_.value0.value5 instanceof Ir?(D=!0,r(_.value1)(new Ir(_.value0.value0,_.value0.value1,_.value0.value2,new er(k,_.value0.value3,_.value0.value4,_.value0.value5.value0),_.value0.value5.value1,_.value0.value5.value2,new er(_.value0.value5.value3,_.value0.value5.value4,_.value0.value5.value5,_.value0.value5.value6)))):_.value0 instanceof of&&_.value0.value3 instanceof Ir?(D=!0,r(_.value1)(new Ir(_.value0.value0,_.value0.value1,_.value0.value2,new er(_.value0.value3.value0,_.value0.value3.value1,_.value0.value3.value2,_.value0.value3.value3),_.value0.value3.value4,_.value0.value3.value5,new er(_.value0.value3.value6,_.value0.value4,_.value0.value5,k)))):(D=!0,Fu("The impossible happened in partial function `up`."))}throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): "+[_.constructor.name])}for(;!D;)A=b(v,m);return A}},a=function(f){return function(m){var v=f,D=!1,A;function b(_,k){if(k instanceof er&&k.value0 instanceof Lt&&k.value3 instanceof Lt)return D=!0,u(_)(Lt.value);if(k instanceof er){v=new Mt(new mo(k.value0,k.value1,k.value2),_),m=k.value3;return}if(k instanceof Ir&&k.value0 instanceof Lt&&k.value3 instanceof Lt&&k.value6 instanceof Lt)return D=!0,u(new Mt(new mo(Lt.value,k.value1,k.value2),_))(Lt.value);if(k instanceof Ir){v=new Mt(new of(k.value0,k.value1,k.value2,k.value3,k.value4,k.value5),_),m=k.value6;return}return D=!0,Fu("The impossible happened in partial function `removeMaxNode`.")}for(;!D;)A=b(v,m);return A}},o=function(f){var m=!1,v;function D(A){if(A instanceof er&&A.value3 instanceof Lt)return m=!0,{key:A.value1,value:A.value2};if(A instanceof er){f=A.value3;return}if(A instanceof Ir&&A.value6 instanceof Lt)return m=!0,{key:A.value4,value:A.value5};if(A instanceof Ir){f=A.value6;return}return m=!0,Fu("The impossible happened in partial function `maxNode`.")}for(;!m;)v=D(f);return v},i=function(f){return function(m){var v=f,D=!1,A;function b(_,k){if(k instanceof Lt)return D=!0,P.value;if(k instanceof er){var G=e(n)(k.value1);if(k.value3 instanceof Lt&&G instanceof Qt)return D=!0,new F(new U(k.value2,u(_)(Lt.value)));if(G instanceof Qt){var nt=o(k.value0);return D=!0,new F(new U(k.value2,a(new Mt(new uf(nt.key,nt.value,k.value3),_))(k.value0)))}if(G instanceof It){v=new Mt(new uf(k.value1,k.value2,k.value3),_),m=k.value0;return}v=new Mt(new mo(k.value0,k.value1,k.value2),_),m=k.value3;return}if(k instanceof Ir){var bt=function(){return k.value0 instanceof Lt&&k.value3 instanceof Lt&&k.value6 instanceof Lt}(),G=e(n)(k.value4),vr=e(n)(k.value1);if(bt&&vr instanceof Qt)return D=!0,new F(new U(k.value2,r(_)(new er(Lt.value,k.value4,k.value5,Lt.value))));if(bt&&G instanceof Qt)return D=!0,new F(new U(k.value5,r(_)(new er(Lt.value,k.value1,k.value2,Lt.value))));if(vr instanceof Qt){var nt=o(k.value0);return D=!0,new F(new U(k.value2,a(new Mt(new af(nt.key,nt.value,k.value3,k.value4,k.value5,k.value6),_))(k.value0)))}if(G instanceof Qt){var nt=o(k.value3);return D=!0,new F(new U(k.value5,a(new Mt(new Ei(k.value0,k.value1,k.value2,nt.key,nt.value,k.value6),_))(k.value3)))}if(vr instanceof It){v=new Mt(new af(k.value1,k.value2,k.value3,k.value4,k.value5,k.value6),_),m=k.value0;return}if(vr instanceof jt&&G instanceof It){v=new Mt(new Ei(k.value0,k.value1,k.value2,k.value4,k.value5,k.value6),_),m=k.value3;return}v=new Mt(new of(k.value0,k.value1,k.value2,k.value3,k.value4,k.value5),_),m=k.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): "+[k.constructor.name])}for(;!D;)A=b(v,m);return A}};return i(wr.value)}},Un={foldr:function(t){return function(r){return function(e){if(e instanceof Lt)return r;if(e instanceof er)return Xr(Un)(t)(t(e.value2)(Xr(Un)(t)(r)(e.value3)))(e.value0);if(e instanceof Ir)return Xr(Un)(t)(t(e.value2)(Xr(Un)(t)(t(e.value5)(Xr(Un)(t)(r)(e.value6)))(e.value3)))(e.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): "+[e.constructor.name])}}},foldl:function(t){return function(r){return function(e){if(e instanceof Lt)return r;if(e instanceof er)return Qr(Un)(t)(t(Qr(Un)(t)(r)(e.value0))(e.value2))(e.value3);if(e instanceof Ir)return Qr(Un)(t)(t(Qr(Un)(t)(t(Qr(Un)(t)(r)(e.value0))(e.value2))(e.value3))(e.value5))(e.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): "+[e.constructor.name])}}},foldMap:function(t){var r=Ut(t),e=tt(t.Semigroup0());return function(n){return function(u){if(u instanceof Lt)return r;if(u instanceof er)return e(ge(Un)(t)(n)(u.value0))(e(n(u.value2))(ge(Un)(t)(n)(u.value3)));if(u instanceof Ir)return e(ge(Un)(t)(n)(u.value0))(e(n(u.value2))(e(ge(Un)(t)(n)(u.value3))(e(n(u.value5))(ge(Un)(t)(n)(u.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): "+[u.constructor.name])}}}},Bn={foldrWithIndex:function(t){return function(r){return function(e){if(e instanceof Lt)return r;if(e instanceof er)return $i(Bn)(t)(t(e.value1)(e.value2)($i(Bn)(t)(r)(e.value3)))(e.value0);if(e instanceof Ir)return $i(Bn)(t)(t(e.value1)(e.value2)($i(Bn)(t)(t(e.value4)(e.value5)($i(Bn)(t)(r)(e.value6)))(e.value3)))(e.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 147, column 26 - line 150, column 120): "+[e.constructor.name])}}},foldlWithIndex:function(t){return function(r){return function(e){if(e instanceof Lt)return r;if(e instanceof er)return Xa(Bn)(t)(t(e.value1)(Xa(Bn)(t)(r)(e.value0))(e.value2))(e.value3);if(e instanceof Ir)return Xa(Bn)(t)(t(e.value4)(Xa(Bn)(t)(t(e.value1)(Xa(Bn)(t)(r)(e.value0))(e.value2))(e.value3))(e.value5))(e.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 151, column 26 - line 154, column 120): "+[e.constructor.name])}}},foldMapWithIndex:function(t){var r=Ut(t),e=tt(t.Semigroup0());return function(n){return function(u){if(u instanceof Lt)return r;if(u instanceof er)return e(vo(Bn)(t)(n)(u.value0))(e(n(u.value1)(u.value2))(vo(Bn)(t)(n)(u.value3)));if(u instanceof Ir)return e(vo(Bn)(t)(n)(u.value0))(e(n(u.value1)(u.value2))(e(vo(Bn)(t)(n)(u.value3))(e(n(u.value4)(u.value5))(vo(Bn)(t)(n)(u.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 155, column 26 - line 158, column 128): "+[u.constructor.name])}}},Foldable0:function(){return Un}},RI=$i(Bn),LI=Xa(Bn),eS=function(){return RI(function(t){return function(r){return function(e){return new Mt(t,e)}}})(wr.value)}();var uc=function(){return Lt.value}();var gb=function(t){var r=II(t);return function(e){return function(n){return gt(n)(Ne)(r(e)(n))}}};var ac=function(t){var r=Xv(t),e=gb(t),n=Up(t);return function(u){return function(a){return function(o){var i=u(r(a)(o));if(i instanceof P)return e(a)(o);if(i instanceof F)return n(a)(i.value0)(o);throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): "+[i.constructor.name])}}}};var NI=function(t){var r=ac(t);return function(e){return function(n){return function(u){var a=function(o){return function(i){return function(f){return r(function(){var m=gt(f)(e(f));return function(v){return F.create(m(v))}}())(o)(i)}}};return LI(a)(u)(n)}}}};var nS=function(t){return NI(t)(w)};var Qv=function(t){return t.partitionMap};var Do=function(t){return t.filterMap};var qp=function(t){return t.filter};var oS=t=>{for(var r=0,e=t.length;r<e;r++)t[r]()},Kv=(t,r)=>{for(var e=0,n=t.length;e<n;e++)r(t[e])},hb=(t,r)=>{for(let e in t)r(t[e])},Sb=()=>({}),xb=(t,r,e)=>{e[t]=r},Cb=(t,r)=>{delete r[t]};var fS=j;var ba={liftST:fS,Monad0:function(){return ao}},Aa=function(t){return t.liftST};var JI=ge(vu),jI=Qr(vu),XI=Xr(vu);var QI=function(t){var r=nS(t);return function(e){return function(n){return r(e)(n)}}};var $b=function(t){return eS(t)};var pS=function(t){return Zh(t)(void 0)};var Tb=function(t){return{append:QI(t)}};var _S=function(t){return tS(t)},vS=function(t){var r=Up(t);return function(e){return function(n){return r(e)(void 0)(n)}}};var sS={foldMap:function(t){var r=JI(t);return function(e){var n=r(e);return function(u){return n($b(u))}}},foldl:function(t){return function(r){var e=jI(t)(r);return function(n){return e($b(n))}}},foldr:function(t){return function(r){var e=XI(t)(r);return function(n){return e($b(n))}}}};var Fb=uc;var mS=function(t){var r=Tb(t);return{mempty:Fb,Semigroup0:function(){return r}}};var Yv=function(t){var r=gb(t);return function(e){return function(n){return r(e)(n)}}};function DS(t){return function(r){return function(){return setTimeout(r,t)}}}function bS(t){return function(){clearTimeout(t)}}var YI=Rt(Te);var Zv=DS;var ZI={eq:function(t){return function(r){return t===r}}},ts={compare:function(t){return function(r){return YI(t)(r)}},Eq0:function(){return ZI}};var zp=bS;var bo=function(r){return function(e){return r(e)()}};var jc=function(r){return function(e){return function(){return r(e)}}};var DR=function(t){var r=tt(ND(t));return{append:function(e){return function(n){return bo(function(u){return r(jc(e)(u))(jc(n)(u))})}}}};var yS=function(t){var r=Ut(io(t)),e=DR(t.Semigroup0());return{mempty:bo(function(n){return r}),Semigroup0:function(){return e}}};var bR=K(Z),AR=S(lo),kR=M(no),Ao=function(t){return t.sampleOnRight};var ka=function(t){return t.keepLatest};var Xc=function(t){return t.fix},da=function(t){var r=Xc(t),e=Ao(t),n=t.Alternative0(),u=ht(n.Plus1().Alt0()),a=M(n.Applicative0()),o=S(t.Filterable1().Functor1());return function(i){return function(f){return function(m){return r(function(v){return e(u(v)(a(f)))(o(Jt(i))(m))})}}}};var Qc=function(t){var r=Do(t.Filterable1()),e=da(t);return function(n){return function(u){return function(a){return r(Ne)(e(function(o){return function(i){return AR(kR)(n(o.value0)(i))}})(new U(u,P.value))(a))}}}},rs=function(t){var r=Do(t.Filterable1()),e=da(t);return function(n){var u=function(a){return function(o){if(a instanceof P)return new F({now:o,last:P.value});if(a instanceof F)return new F({now:o,last:new F(a.value0.now)});throw new Error("Failed pattern match at FRP.Event.Class (line 83, column 3 - line 83, column 50): "+[a.constructor.name,o.constructor.name])}};return r(bR)(e(u)(P.value)(n))}};function Mb(t){return function(r){return t===r}}var Ob=Mb;var SS=function(t,r,e){var n=0,u;return function(a){if(n===2)return u;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+a+")",r,a);return n=1,u=e(),n=2,u}},xS=$u(Wt),ns=xS(Me),fc=M(Wt),Za=Aa(ba),gR=io(uo),Jp=Pr(L),hR=tt(He),CS=Ut(yS(uo)),SR=Ut(mS(ts)),xR=Yv(ts),CR=tt(Tb(ts)),$R=xS(sS),TR=it(or),FR=S(L);var ER=function(t){return function(r){return function(e,n){var u=Or(P.value)(),a=t(e,function(i){return te(new F(i))(u)()}),o=r(e,function(i){var f=Ur(u)();return ns(f)(function(m){return function(){return n(i(m))}})()});return function(){return a(),o()}}}},MR=function(t){return function(r){return function(e,n){var u=Or(P.value)(),a=t(e,function(i){var f=Ur(u)();return ns(f)(function(m){return function(){return n(m(i))}})()}),o=r(e,function(i){return te(new F(i))(u)()});return function(){return a(),o()}}}},cf=function(t){var r=ge(t)(gR);return function(e){return function(n,u){var a=Za(Yo)();return r(function(o){return function(){var f=o(n,u);return Jp(Za(co(f)(a)))()}})(e)(),function(){var i=Za(dp(a))();return oS(i)}}}},hS=function(t){var r=ac(t),e=Xv(t);return function(){var u=Or(uc)();return{event:function(a){return function(o,i){return Jp(Yf(r(function(f){if(f instanceof P)return new F([i]);if(f instanceof F)return new F(hR(f.value0)([i]));throw new Error("Failed pattern match at FRP.Event (line 568, column 17 - line 570, column 51): "+[f.constructor.name])})(a))(u))(),Jp(Yf(r(function(f){if(f instanceof P)return P.value;if(f instanceof F)return new F(_b(Ob)(i)(f.value0));throw new Error("Failed pattern match at FRP.Event (line 577, column 17 - line 579, column 65): "+[f.constructor.name])})(a))(u))}},push:function(a){var o=Ur(u)(),i=e(a.address)(o);if(i instanceof P)return void 0;if(i instanceof F)return Kv(i.value0,function(f){return f(a.payload)});throw new Error("Failed pattern match at FRP.Event (line 586, column 9 - line 588, column 95): "+[i.constructor.name])}}}},OR=function(t){return function(r,e){var n=Or(fc(void 0))(),u=t(r,function(a){var o=Ur(n)();o();var i=a(r,e);return te(i)(n)()});return function(){var o=Ur(n)();return o(),u()}}},ct={map:function(t){return function(r){return function(e,n){return r(e,function(u){return n(t(u))})}}}},wR=S(ct),Gp=function(t){return function(r){return function(e,n){return r(e,function(u){var a=t(u);if(a instanceof F)return n(a.value0);if(a instanceof P)return void 0;throw new Error("Failed pattern match at FRP.Event (line 225, column 31 - line 227, column 35): "+[a.constructor.name])})}}},wb=function(t){return Gp(function(r){var e=t(r);if(e)return new F(r);if(!e)return P.value;throw new Error("Failed pattern match at FRP.Event (line 141, column 13 - line 143, column 25): "+[e.constructor.name])})},es=function(){var r=Sb(),e=Or(0)();return{event:function(n,u){var a=Or(u)(),o=Ur(e)();return xb(o,a,r),oo(function(i){return i+1|0})(e)(),function(){return te(CS)(a)(),Cb(o,r),void 0}},push:function(n){return hb(r,function(u){var a=Ur(u)();return a(n)})}}},PR=function(t){return function(r,e){var n=es(),u=t(n.event),a=n.event(r,e),o=u(r,n.push);return function(){return o(),a()}}},$S={compact:Gp(K(Z)),separate:function(t){return{left:Gp(function(r){if(r instanceof xt)return new F(r.value0);if(r instanceof Ct)return P.value;throw new Error("Failed pattern match at FRP.Event (line 124, column 13 - line 126, column 33): "+[r.constructor.name])})(t),right:Gp(function(r){if(r instanceof Ct)return new F(r.value0);if(r instanceof xt)return P.value;throw new Error("Failed pattern match at FRP.Event (line 131, column 13 - line 133, column 32): "+[r.constructor.name])})(t)}}},ya={filter:wb,filterMap:Gp,partition:function(t){return function(r){return{yes:wb(t)(r),no:wb(function(e){return!t(e)})(r)}}},partitionMap:function(t){return function(r){return{left:Do(ya)(function(){var e=jn(F.create)(w(P.value));return function(n){return e(t(n))}}())(r),right:Do(ya)(function(e){return Fv(t(e))})(r)}}},Compactable0:function(){return $S},Functor1:function(){return ct}},IR=function(t){return function(r){return function(e,n){var u=Or(P.value)(),a=Za(Yo)(),o=Or(P.value)(),i=Za(Yo)(),f=Or(!0)(),m=t(e,function(b){var _=Ur(f)();if(_)return Jp(Za(co(b)(a)))();te(new F(b))(u)();var k=Ur(o)();return ns(k)(function(G){return function(){return n(G(b))}})()}),v=r(e,function(b){var _=Ur(f)();if(_)return Jp(Za(co(b)(i)))();te(new F(b))(o)();var k=Ur(u)();return ns(k)(function(G){return function(){return n(b(G))}})()});te(!1)(f)();var D=Za(dp(a))(),A=Za(dp(i))();return function(){return D.length===0?te(lb(A))(o)():Kv(D,function(b){return te(new F(b))(u)(),Kv(A,function(_){return te(new F(_))(o)(),n(_(b))})})}(),Za(Ov(0)(Vn(D))([])(a))(),Za(Ov(0)(Vn(A))([])(i))(),function(){return m(),v()}}}},we=function(t){return function(r){return r}(Ib(345).subscribe)(t)},Ib=SS("backdoor","FRP.Event",function(){var t=function(){var e=Sb(),n=Or(0)();return{event:function(u,a){var o=Or(a)(),i=Ur(n)();return xb(i,o,e),oo(function(f){return f+1|0})(n)(),function(){return te(CS)(o)(),Cb(i,e),void 0}},push:function(u){return function(){return hb(e,function(a){var o=Ur(a)();return o(u)})}}}};return{createO:es,makeEvent:function(){var r=function(e){return function(n,u){return n?fc(void 0):e(function(a){return function(){return u(a)}})()}};return r}(),makeEventO:function(){var r=function(e){return function(n,u){return n?fc(void 0):e(u)}};return r}(),makePureEvent:function(){var r=function(e){return function(n,u){return e(function(a){return function(){return u(a)}})()}};return r}(),makeLemmingEvent:function(){var r=function(e){return function(n,u){var a=function(o){return function(i){return function(){return o(n,bo(i))}}};return e(a)(function(o){return function(){return u(o)}})()}};return r}(),makeLemmingEventO:function(){var r=function(e){return function(n,u){var a=function(o,i){return o(n,i)};return e(a,u)}};return r}(),create:t,createPure:t,createPureO:es,subscribe:function(){var r=function(e){return function(n){return function(){return e(!1,bo(n))}}};return r}(),subscribeO:function(){var r=function(e,n){return e(!1,n)};return r}(),subscribePureO:function(){var r=function(e,n){return e(!0,n)};return r}(),subscribePure:function(){var r=function(){var e=function(n){return function(u){return function(){return n(!0,bo(u))}}};return e}();return r}(),bus:function(){var r=function(e){return function(n,u){var a=Pb(819)();return u(e(a.push)(a.event)),fc(void 0)}};return r}(),memoize:function(){var r=function(e){return function(n){return function(u,a){var o=es();return a(n(o.event)),e(u,o.push)}}};return r}(),hot:function(){var r=function(e){return function(){var u=Pb(837)(),a=we(e)(u.push)();return{event:u.event,unsubscribe:a}}};return r}(),mailbox:function(){var r=function(e){return function(){var u=hS(e)();return{event:u.event,push:function(a){return function(){return u.push(a)}}}}};return r}(),mailboxed:function(){var r=function(e){var n=hS(e);return function(u){return function(a){return function(o,i){var f=n();return i(a(f.event)),u(o,f.push)}}}};return r}(),delay:function(){var r=function(e){return function(n){return function(u,a){var o=Or(SR)(),i=n(u,function(f){var m=Or(P.value)(),v=Zv(e)(function(){a(f);var A=Ur(m)();return gt(fc(void 0))(function(b){return oo(xR(b))(o)})(A)()})();return te(new F(v))(m)(),oo(CR(pS(v)))(o)()});return function(){var m=Ur(o)();return $R(m)(zp)(),i()}}}};return r}()}}),Pb=SS("create","FRP.Event",function(){return function(){return void 0,function(r){return r}(Ib(461).create)()}}),Kc=Ib(678),TS=Pb(458),Rb=function(t){return function(r){return r}(Kc.bus)(t)};var ga=function(t){return function(r){return r}(Kc.delay)(t)};var Cn=function(t){return function(r){return r}(Kc.makeEvent)(t)};var Lb=function(t){return function(r){return r}(Kc.makeLemmingEvent)(t)},mr=function(t){return function(r){return r}(Kc.makeLemmingEventO)(t)};var ti=function(t){return function(r){return r}(Kc.memoize)(t)};var pn={apply:function(t){return function(r){return IR(t)(wR(Jf)(r))}},Functor0:function(){return ct}},RR=$e(pn),LR=function(t){return{append:RR(tt(t))}},X={pure:function(t){return function(r,e){return e(t),fc(void 0)}},Apply0:function(){return pn}},NR=M(X);var FS=function(t){var r=LR(t.Semigroup0());return{mempty:NR(Ut(t)),Semigroup0:function(){return r}}};var qt={alt:function(t){return function(r){return function(e,n){return TR(FR(function(u){return function(a){return function(){return u(),a()}}})(function(){return t(e,n)}))(function(){return r(e,n)})()}}},Functor0:function(){return ct}};var h={empty:function(t,r){return fc(void 0)},Alt0:function(){return qt}},UR={Applicative0:function(){return X},Plus1:function(){return h}},me={keepLatest:OR,sampleOnRight:ER,sampleOnLeft:MR,fix:PR,Alternative0:function(){return UR},Filterable1:function(){return ya}};var BR=function(t){return t},jp=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Xp=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),us=function(){function t(){}return t.value=new t,t}(),WR=function(t){return t};var as=tn(),c=WR;var s=function(){return jp.create}();var J=function(){return Xp.create}(),Vr=function(){var t=S(Qe)(S(L)(w(!0)));return function(r){return BR(t(r))}}(),mt=function(t){return t.attr};function ES(t){return()=>t.slice()}function MS(t){return r=>e=>()=>{e[t]=r}}function OS(t){return()=>t.slice()}function Qp(t,r){var e={};for(var n in r)({}).hasOwnProperty.call(r,n)&&(e[n]=r[n]);for(var u in t)({}).hasOwnProperty.call(t,u)&&(e[u]=t[u]);return e}var PS=function(t){var r=fe(t);return function(){return function(){return function(e){return function(n){return function(u){return qa(r(e))(n)(u)}}}}}};var IS=function(){return function(){return function(t){return function(r){return Qp(t,r)}}}},Kp=function(t){var r=fe(t);return function(){return function(){return function(e){return function(n){return function(u){return qa(r(e))(n)(u)}}}}}},ko=function(t){var r=fe(t);return function(){return function(e){return function(n){return Au(r(e))(n)}}}};var RS=M(Ee);var Yn={vb:function(t){return RS(new U({},{}))}},is=function(t){return t.vb},JR={vbus:function(){var t=function(){return function(n){var u=is(n);return function(a){return function(o){return Lb(function(i){return function(f){return function(){var v=u(d.value)();return f(o(v.value0)(v.value1))(),RS(void 0)}}})}}}},r=t(),e=function(){return function(n){return r(n)}};return e}()},Ub=function(){return function(t){return function(r){return function(e){return e()(t)}(JR.vbus)(r)}}},Mi=function(t){var r=Kp(t)()();return function(){return function(){return function(){return function(e){var n=is(e);return function(u){var a=is(u);return function(){return function(){return{vb:function(o){return function(){var f=a(d.value)(),m=n(d.value)();return new U(r(d.value)(m.value0)(f.value0),r(d.value)(m.value1)(f.value1))}}}}}}}}}}},De=function(t){var r=Kp(t)()();return function(){return function(){return function(e){var n=is(e);return function(){return function(){return{vb:function(u){return function(){var o=n(d.value)(),i=TS();return new U(r(d.value)(i.push)(o.value0),r(d.value)(i.event)(o.value1))}}}}}}}}};var Eu=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),lf=function(){function t(){}return t.value=new t,t}();var Yc=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Zc=function(){function t(){}return t.value=new t,t}(),Bb=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Wb=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Yp=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),ri=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),_r=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var LS=function(t){return t};var go={eq:function(t){return function(r){return t instanceof Eu&&r instanceof Eu?t.value0===r.value0:t instanceof lf&&r instanceof lf}}};var ze=function(t){return new Yp(t)},tl=function(t){return new ri(t)},Zp=function(t){return new Wb(t)};var NS=t=>r=>r[t];var XR=function(){function t(o){this.fn=o}var r={},e=function(o,i){this.head=o,this.tail=i};function n(o){return new e(o,r)}function u(o){return function(i){return new e(o,i)}}function a(o){for(var i=[],f=o;f!==r;)i.push(f.head),f=f.tail;return i}return function(o){return function(i){return function(f){var m=function(D,A){return o(i(u)(f(D)))(A)},v=function(D,A,b){if(A===0)return D;var _=b[A-1];return new t(function(){var k=v(m(_,D),A-1,b);return k})};return function(D){for(var A=i(n)(f(D[D.length-1])),b=v(A,D.length-1,D);b instanceof t;)b=b.fn();return i(a)(b)}}}}}();var BS=function(t){return t};var WS=He;var qS=Ht;var lL=zc();var QS=BS;var el=function(t){return t};var t_=function(t){return QS(Lh(t))};var nl=function(t){if(Vn(t)>0)return new F(QS(t));if(Jr)return P.value;throw new Error("Failed pattern match at Data.Array.NonEmpty (line 161, column 1 - line 161, column 58): "+[t.constructor.name])};var KS=function(t){return function(r){return t(el(r))}};var YS=KS(Vn);var ZS=function(){return KS(lL)};var lc={proof:function(t){return t},Coercible0:function(){}},tx=function(t){return t.proof};var pc=function(t){return t.reflectType};var ex=function(t){return pc(t)};var e_=ir;var ps=function(){return function(t){return t}};var Hb=function(t){return[t]};var zb=function(t){var r=ex(t);return function(){return function(){return function(){return function(e){return NS(r(e))}}}}};var Vb=[];var nx=function(){return function(){return function(t){return function(r){return tf(t)(r)}}}};function ux(t){return function(){var r={};for(var e in t)hasOwnProperty.call(t,e)&&(r[e]=t[e]);return r}}var wi={};function Gb(t){return t()}function ax(t,r){var e={};for(var n in t)hasOwnProperty.call(t,n)&&(e[n]=r(t[n]));return e}function ix(t,r){var e={};for(var n in t)hasOwnProperty.call(t,n)&&(e[n]=r(n)(t[n]));return e}function ox(t){return function(r){return function(e){return function(n){var u=e;function a(i){return function(f){return r(f)(i)(n[i])}}for(var o in n)hasOwnProperty.call(n,o)&&(u=t(u)(a(o)));return u}}}}function n_(t){return function(r){var e=[];for(var n in r)hasOwnProperty.call(r,n)&&e.push(t(n)(r[n]));return e}}var mL=Object.keys||n_(function(t){return function(){return t}});function Jb(t){return function(r){return function(e){return function(){return e[t]=r,e}}}}var jb=function(t){return function(r){return function(){return delete r[t],r}}};var fx=Xr(Ht),yL=K(Z);var Xb=n_(function(t){return function(r){return r}});var gL=ux;var cx=function(t){return function(r){return Gb(function(){var n=gL(r)();return t(n)(),n})}};var lx=function(t){return function(r){return ix(r,t)}};var So=function(t){return function(r){return cx(Jb(t)(r))}},vs={map:function(t){return function(r){return ax(r,t)}}},hL={mapWithIndex:lx,Functor0:function(){return vs}},Qb=function(){return j};var ss=ox(Jf),px=function(t){var r=tt(t.Semigroup0()),e=Ut(t);return function(n){return ss(function(u){return function(a){return function(o){return r(u)(n(a)(o))}}})(e)}},u_={foldl:function(t){return ss(function(r){return function(e){return t(r)}})},foldr:function(t){return function(r){return function(e){return fx(t)(r)(Xb(e))}}},foldMap:function(t){var r=px(t);return function(e){return r(w(e))}}},_x={foldlWithIndex:function(t){return ss(Jt(t))},foldrWithIndex:function(t){return function(r){return function(e){return fx(qc(t))(r)(n_(U.create)(e))}}},foldMapWithIndex:function(t){return px(t)},Foldable0:function(){return u_}},SL={traverseWithIndex:function(t){var r=t.Apply0(),e=it(r),n=S(r.Functor0()),u=M(t);return function(a){return function(o){return ss(function(i){return function(f){return function(m){return e(n(Jt(So(f)))(i))(a(f)(m))}}})(u(wi))(o)}}},FunctorWithIndex0:function(){return hL},FoldableWithIndex1:function(){return _x},Traversable2:function(){return ul}},ul={traverse:function(t){var r=rf(SL)(t);return function(e){return r(w(e))}},sequence:function(t){return en(ul)(t)(yL)},Functor0:function(){return vs},Foldable1:function(){return u_}};var Kb=function(t){return cx(jb(t))};function vx(t){var r={};for(var e in t)({}).hasOwnProperty.call(t,e)&&(r[e]=t[e]);return r}function sx(t){return function(r){return function(e){return e[t]=r,e}}}function mx(t){return function(r){return function(e){return e[t]=r(e[t]),e}}}var a_=gi;var Dx=function(){return function(){return function(t){var r=fe(t);return function(e){return function(n){return function(u){return mx(r(e))(n)(u)}}}}}};var i_=function(){return function(){return function(t){var r=fe(t);return function(e){return function(n){return function(u){return sx(r(e))(n)(u)}}}}}};var Yb=Z,ms=function(t){return function(r){return t(vx(r))}},bx=Jt(ms)({});var CL=ka(me),$L=S(ct),dx=cf(Ht);var TL=lt(Nc),FL=W(ir),tA=ps(),EL=x(h),ML=Tu(Da),OL=ED(Cg),xo=M(Ee),yx=S(Kn),wL=S(e_),gx=$u(Ee),eA=gx(Me),PL=ip(a_),IL=i_()()({reflectSymbol:function(){return"id"}}),RL=Dx()()({reflectSymbol:function(){return"parent"}}),Sa=Pr(Kn),hx=Pn(Ee),Sx=gx(Ht),LL=K(Z),NL=S(ir),Ax=tt(qD($v)),UL=Dn(Ee)(Ht),BL=tt(He),WL=Qr(u_),qL=rr(Mv),kx=function(){function t(){}return t.value=new t,t}(),rA=function(){function t(){}return t.value=new t,t}(),HL=function(){function t(){}return t.value=new t,t}();var Zn=function(t){return function(r){return function(e){var n=function(u){return u(r)(e)};return function(u){if(u instanceof Yp)return dx(NL(Zn(t)(r)(e))(u.value0));if(u instanceof ri)return CL($L(Zn(t)(r)(e))(u.value0));if(u instanceof _r)return n(t.toElt(u.value0));if(u instanceof Wb)return mr(function(a,o){var i=Fe(wi)(),f=a(u.value0,function(m){var v=t.ids(e)(),D=Fe(xo(void 0))(),A=t.ids(e)(),b=Fe(xo(void 0))(),_=Fe([])(),k=Fe(xo(void 0))(),G=yx(Eu.create)(function(){if(r.scope instanceof lf)return t.ids(e);if(r.scope instanceof Eu)return Ax(xo(r.scope.value0))(Ax(xo("!"))(t.ids(e)));throw new Error("Failed pattern match at Bolson.Control (line 547, column 17 - line 549, column 67): "+[r.scope.constructor.name])}())(),nt=Fe(kx.value)(),bt=a(m,function(Gr){var z=Ze(nt)();if(Gr instanceof Bb&&z instanceof rA){var zt=Ze(_)();return UL(function(Wr){return function(){return o(t.doLogic(Gr.value0)(e)(Wr))}})(zt)()}if(Gr instanceof Zc&&z instanceof rA){Sa(Cu(HL.value)(nt))();var st=function(){var fn=Ze(_)();Sx(fn)(function(be){return eA(r.parent)(function(ue){return function(){return o(t.disconnectElement(e)({id:be,parent:ue,scope:G}))}})})();var On=Ze(D)();On();var ia=Ze(b)();return ia(),Sa(ja(Kb(v))(i))(),Sa(ja(Kb(A))(i))()};return Sa(Cu(st)(k))(),st()}if(Gr instanceof Yc&&z instanceof kx){Sa(Cu(rA.value)(nt))();var qr=a(Zn(t)(function(){var Wr={};for(var fn in r)({}).hasOwnProperty.call(r,fn)&&(Wr[fn]=r[fn]);return Wr.scope=G,Wr.raiseId=function(On){return Sa(ja(BL([On]))(_))},Wr}())(e)(Gr.value0),o);return Sa(ja(So(A)(qr))(i))(),Sa(Cu(qr)(b))()}return void 0});Sa(Cu(bt)(D))(),Sa(ja(So(v)(bt))(i))();var vr=Ze(k)();return vr()});return function(){return TL(Ze(i))(WL(qL)(xo(void 0)))(),f()}});throw new Error("Failed pattern match at Bolson.Control (line 520, column 17 - line 610, column 20): "+[u.constructor.name])}}}},zL=function(){return function(){return function(){return function(t){return function(r){return function(e){return function(n){return function(u){return function(a){var o=function(i){return function(f){return mr(function(m,v){var D=ES(FL(tA(u))({id:"",entity:new ri(EL)}))(),A=dx(ML(function(vr){return OL(function(Gr){return function(z){return z instanceof _r?function(zt){return zt(function(){var st={};for(var qr in i)({}).hasOwnProperty.call(i,qr)&&(st[qr]=i[qr]);return st.parent=P.value,st.scope=r(i.scope),st.raiseId=function(Wr){return MS(vr)({id:Wr,entity:z})(D)},st}())(f)}(n.toElt(z.value0)):Gr(n.wrapElt(z))}})})(tA(u))),b=m(A,v),_=Fe(xo(void 0))(),k=yx(j)(OS(D))(),G=wL(function(vr){return function(Gr){return new _r(n.fromEltO1(function(z){return function(zt){return mr(function(st,qr){return z.raiseId(vr.id)(),eA(z.parent)(function(Wr){return function(){return qr(n.giveNewParent(zt)(ms(PL(IL(d.value)(vr.id))(RL(d.value)(w(Wr))))(z))(vr.entity)(Gr))}})(),xo(void 0)})}}))}})(k),nt=Zn(e)(i)(f)(a(G)),bt=m(nt,v);return Sa(Cu(bt)(_))(),function(){b(),hx(!t)(Sx(tA(k))(function(z){return function(){return v(n.deleteFromCache(f)({id:z.id}))}}))();var Gr=Ze(_)();return Gr()}})}};return new _r(n.fromEltO2(o))}}}}}}}}},VL=zL()()();var nA=function(){return function(){return function(){return function(t){return function(r){return function(e){return function(n){return VL(!1)(LL)(t)(r)(e)(n)}}}}}}};var xx=function(t){return function(r){return function(e){var n=function(u){return function(a){return mr(function(o,i){var f=Fe(P.value)(),m=e(new _r(r.fromElt(function(v){return function(D){return mr(function(A,b){return function(){var k=Ze(f)();if(k instanceof P)return void 0;if(k instanceof F)return eA(v.parent)(function(G){return hx(k.value0!==G)(function(){return v.raiseId(k.value0)(),b(r.connectToParent(a)({id:k.value0,parent:G}))})})();throw new Error("Failed pattern match at Bolson.Control (line 635, column 27 - line 643, column 16): "+[k.constructor.name])}(),xo(void 0)})}})));return o(Zn(t)(function(){var v={};for(var D in u)({}).hasOwnProperty.call(u,D)&&(v[D]=u[D]);return v.parent=u.parent,v.scope=u.scope,v.raiseId=function(A){return function(){return u.raiseId(A)(),Sa(Cu(new F(A))(f))()}},v}())(a)(m),i)})}};return new _r(r.fromElt(n))}}};var GL=K(Z);var Wn={dimap:function(t){return function(r){return function(e){return function(n){return r(e(t(n)))}}}}},Pi=function(t){return t.dimap},Ju=function(t){var r=Pi(t);return function(e){return r(e)(GL)}};var JL=Ub(),il=S(ct),jL=Ju(Wn),XL=ie(),QL=yt(go),KL=tn(),YL=M(Ee),uA=M(X),ZL=x(h),tN=cf(Ht);var Ii=function(){return function(t){var r=JL(t);return function(e){return function(n){var u=r(e)(n),a=il(function(i){return i})(u),o=il(function(i){return i})(a);return new ri(o)}}}};var Cx=function(t){return function(r){var e=function(n){var u=function(a){return a instanceof _r?new _r(jL(function(o){return{pos:t,dynFamily:o.dynFamily,ez:o.ez,parent:o.parent,raiseId:o.raiseId,scope:o.scope}})(a.value0)):a instanceof ri?new ri(il(u)(a.value0)):a};return u(n)};return e(r)}},$x=function(t){return Cx(F.create(t))};var aA=function(){return Zc.value}(),rN=function(){return{doLogic:function(t){return function(r){return function(e){return r.sendToPos({id:e,pos:t})}}},ids:function(t){return function(r){return r.ids}(XL(t))},disconnectElement:function(t){return function(r){return t.disconnectElement({id:r.id,scope:r.scope,parent:r.parent,scopeEq:QL})}},toElt:function(t){return t}}},eN=rN(),Tx=function(t){var r=function(n){return new Yc(n)},e=Cx(P.value)(t);return r(function(n){return n}(e))};var Fx=function(t){return Rb(t)};var Ue=function(t){var r=function(n){return new ri(KL(n))},e=Fx(t);return r(il(function(n){return n})(e))},Ex=function(t){return Ue(Zf(t))},nN=function(t){return function(r){return function(e){return Zn(eN)(t)(r)(function(n){return n}(e))}}},uN=function(t){return function(r){var e=function(u){return function(a){return function(o){return mr(function(i,f){var m=o.ids();a.raiseId(m)();var v=function(){if(a.parent instanceof P){var A=o.ids();return new U(uA(o.makeElement({id:A,parent:P.value,scope:a.scope,tag:"div",pos:P.value,dynFamily:P.value})),A)}if(a.parent instanceof F)return new U(ZL,a.parent.value0);throw new Error("Failed pattern match at Deku.Core (line 451, column 38 - line 467, column 40): "+[a.parent.constructor.name])}(),D=i(tN([v.value0,uA(o.makeDynBeacon({id:m,parent:new F(v.value1),scope:a.scope,dynFamily:a.dynFamily,pos:a.pos})),uA(o.attributeParent({id:m,parent:v.value1,pos:a.pos,dynFamily:a.dynFamily,ez:a.ez})),nN({parent:new F(v.value1),scope:a.scope,ez:!1,raiseId:function(A){return YL(void 0)},pos:P.value,dynFamily:new F(m)})(o)(u)]),f);return function(){return f(o.removeDynBeacon({id:m})),D()}})}}},n=function(u){return new _r(e(u))};return n(function(u){return u}(t(r)))}},Mx=function(){var t=function(e){return Zp(e)},r=function(e){return t(il(il(function(n){return n}))(e))};return uN(r)}();var Ds=S(ct);var iA=cf(Ht),_c=M(X),iN=x(h),wx=M(Ee),oN=Qc(me),fN=ka(me),cN=qp(ya),lN=yt(eo),Px=tn(),pN=ie(),_N=yt(go),Ix=ht(qt),vN=tt(He),sN=Tu(Da),Ox=S(Qe);var mN=function(t){return function(r){return function(e){return Ds(function(n){return t.setText(function(u){return{id:r,text:u}}(n))})(e)}}},DN=function(t){return function(r){return function(e){return Ds(function(n){return function(u){if(u.value instanceof jp)return t.setProp({id:r,key:u.key,value:u.value.value0});if(u.value instanceof Xp)return t.setCb({id:r,key:u.key,value:u.value.value0});if(u.value instanceof us)return t.unsetAttribute({id:r,key:u.key});throw new Error("Failed pattern match at Deku.Control (line 72, column 28 - line 75, column 47): "+[u.value.constructor.name])}(as(n))})(e)}}};var Ve=function(t){var r=function(n){return function(u){return mr(function(a,o){var i=u.ids();n.raiseId(i)();var f=a(iA([_c(u.makeText({id:i,parent:n.parent,pos:n.pos,scope:n.scope,dynFamily:n.dynFamily})),mN(u)(i)(t),gt(iN)(function(m){return _c(u.attributeParent({id:i,parent:m,pos:n.pos,dynFamily:n.dynFamily,ez:n.ez}))})(n.parent)]),o);return function(){return o(u.deleteFromCache({id:i})),f()}})}},e=new _r(r);return e},C=function(t){return Ve(_c(t))},Rx=function(t){return function(r){var e=function(){var n=function(u){return function(a){return new U(u+1|0,new U(a,u))}};return oN(n)(0)}();return Mx(fN(ti(e(r))(function(n){return Ds(function(u){return iA([Ds(w(aA))(cN(function(){var a=lN(u.value1+1|0);return function(o){return a(Ne(o))}}())(n)),_c(Tx(Px(t(u.value0))))])})(n)})))}};var bN=function(){return{doLogic:function(t){return function(r){return function(e){return r.sendToPos({id:e,pos:t})}}},ids:function(t){return function(r){return r.ids}(pN(t))},disconnectElement:function(t){return function(r){return t.disconnectElement({id:r.id,scope:r.scope,parent:r.parent,scopeEq:_N})}},toElt:function(t){return t}}},AN=bN();var Lx=function(t){return function(r){return function(e){return Zn(AN)(t)(r)(e)}}},Nx=function(t){return function(r){var e=function(n){return function(u){return mr(function(a,o){return a(Ix(_c(u.makeRoot({id:"deku-root",root:t})))(Lx({parent:new F("deku-root"),scope:new Eu("rootScope"),raiseId:function(i){return wx(void 0)},ez:!0,pos:P.value,dynFamily:P.value})(u)(n)),o)})}};return e(r)}},kN=function(t){return function(r){return function(e){var n=function(u){return function(a){return mr(function(o,i){var f=a.ids();u.raiseId(f)();var m=o(Ix(iA(vN([_c(a.makeElement({id:f,parent:u.parent,scope:u.scope,tag:t,pos:u.pos,dynFamily:u.dynFamily})),DN(a)(f)(r)])(gt([])(function(v){return[_c(a.attributeParent({id:f,parent:v,pos:u.pos,dynFamily:u.dynFamily,ez:u.ez}))]})(u.parent))))(Lx({parent:new F(f),scope:u.scope,ez:!0,raiseId:function(v){return wx(void 0)},pos:P.value,dynFamily:P.value})(a)(e)),i);return function(){return i(a.deleteFromCache({id:f})),m()}})}};return n}}},O=function(t){return function(r){return function(e){var n=function(u){return new _r(kN(t)(r)(ze(Px(u))))};return n(sN(Ox(Ox(function(u){return u}))($x))(e))}}};var ee=function(){function t(){}return t.value=new t,t}();var ke={attr:function(t){return function(r){return c({key:"click",value:J(r)})}}};var Ot=function(){function t(){}return t.value=new t,t}();var bs={attr:function(t){return function(r){return c({key:"style",value:s(r)})}}};var Ux={attr:function(t){return function(r){return c({key:"style",value:s(r)})}}};var Ar={attr:function(t){return function(r){return c({key:"style",value:s(r)})}}};var Bx={attr:function(t){return function(r){return c({key:"style",value:s(r)})}}},ol={attr:function(t){return function(r){return c({key:"style",value:s(r)})}}};var Wx={attr:function(t){return function(r){return c({key:"style",value:s(r)})}}};var qx={attr:function(t){return function(r){return c({key:"style",value:s(r)})}}};var oA=O("a");var sr=O("div"),Lr=sr(x(h));var cl=O("span"),fA=cl(x(h));var Li=function(t){return function(r){return t(r)}};var tu=function(){var t={},r="Pure",e="Throw",n="Catch",u="Sync",a="Async",o="Bind",i="Bracket",f="Fork",m="Sequential",v="Map",D="Apply",A="Alt",b="Cons",_="Resume",k="Release",G="Finalizer",nt="Finalized",bt="Forked",vr="Fiber",Gr="Thunk";function z($t,zr,Ie,Yr){this.tag=$t,this._1=zr,this._2=Ie,this._3=Yr}function zt($t){var zr=function(Ie,Yr,Vt){return new z($t,Ie,Yr,Vt)};return zr.tag=$t,zr}function st($t){return new z(r,void 0)}function qr($t){try{$t()}catch(zr){setTimeout(function(){throw zr},0)}}function Wr($t,zr,Ie){try{return zr(Ie())}catch(Yr){return $t(Yr)}}function fn($t,zr,Ie){try{return zr(Ie)()}catch(Yr){return Ie($t(Yr))(),st}}var On=function(){var $t=1024,zr=0,Ie=0,Yr=new Array($t),Vt=!1;function ft(){var jr;for(Vt=!0;zr!==0;)zr--,jr=Yr[Ie],Yr[Ie]=void 0,Ie=(Ie+1)%$t,jr();Vt=!1}return{isDraining:function(){return Vt},enqueue:function(jr){var hr,Ce;zr===$t&&(Ce=Vt,ft(),Vt=Ce),Yr[(Ie+zr)%$t]=jr,zr++,Vt||ft()}}}();function ia($t){var zr={},Ie=0,Yr=0;return{register:function(Vt){var ft=Ie++;Vt.onComplete({rethrow:!0,handler:function(jr){return function(){Yr--,delete zr[ft]}}})(),zr[ft]=Vt,Yr++},isEmpty:function(){return Yr===0},killAll:function(Vt,ft){return function(){if(Yr===0)return ft();var jr=0,hr={};function Ce(ae){hr[ae]=zr[ae].kill(Vt,function(We){return function(){delete hr[ae],jr--,$t.isLeft(We)&&$t.fromLeft(We)&&setTimeout(function(){throw $t.fromLeft(We)},0),jr===0&&ft()}})()}for(var yn in zr)zr.hasOwnProperty(yn)&&(jr++,Ce(yn));return zr={},Ie=0,Yr=0,function(ae){return new z(u,function(){for(var We in hr)hr.hasOwnProperty(We)&&hr[We]()})}}}}}var be=0,ue=1,Wu=2,to=3,Ec=4,lu=5,dn=6;function Vf($t,zr,Ie){var Yr=0,Vt=be,ft=Ie,jr=null,hr=null,Ce=null,yn=null,ae=null,We=0,Gf=0,oa=null,Vo=!0;function Go(Gt){for(var Xt,Nr,Hr;;)switch(Xt=null,Nr=null,Hr=null,Vt){case Wu:Vt=ue;try{ft=Ce(ft),yn===null?Ce=null:(Ce=yn._1,yn=yn._2)}catch(wn){Vt=lu,jr=$t.left(wn),ft=null}break;case to:$t.isLeft(ft)?(Vt=lu,jr=ft,ft=null):Ce===null?Vt=lu:(Vt=Wu,ft=$t.fromRight(ft));break;case ue:switch(ft.tag){case o:Ce&&(yn=new z(b,Ce,yn)),Ce=ft._2,Vt=ue,ft=ft._1;break;case r:Ce===null?(Vt=lu,ft=$t.right(ft._1)):(Vt=Wu,ft=ft._1);break;case u:Vt=to,ft=Wr($t.left,$t.right,ft._1);break;case a:Vt=Ec,ft=fn($t.left,ft._1,function(wn){return function(){Yr===Gt&&(Yr++,On.enqueue(function(){Yr===Gt+1&&(Vt=to,ft=wn,Go(Yr))}))}});return;case e:Vt=lu,jr=$t.left(ft._1),ft=null;break;case n:Ce===null?ae=new z(b,ft,ae,hr):ae=new z(b,ft,new z(b,new z(_,Ce,yn),ae,hr),hr),Ce=null,yn=null,Vt=ue,ft=ft._1;break;case i:We++,Ce===null?ae=new z(b,ft,ae,hr):ae=new z(b,ft,new z(b,new z(_,Ce,yn),ae,hr),hr),Ce=null,yn=null,Vt=ue,ft=ft._1;break;case f:Vt=to,Xt=Vf($t,zr,ft._2),zr&&zr.register(Xt),ft._1&&Xt.run(),ft=$t.right(Xt);break;case m:Vt=ue,ft=ro($t,zr,ft._1);break}break;case lu:if(Ce=null,yn=null,ae===null)Vt=dn,ft=hr||jr||ft;else switch(Xt=ae._3,Hr=ae._1,ae=ae._2,Hr.tag){case n:hr&&hr!==Xt&&We===0?Vt=lu:jr&&(Vt=ue,ft=Hr._2($t.fromLeft(jr)),jr=null);break;case _:hr&&hr!==Xt&&We===0||jr?Vt=lu:(Ce=Hr._1,yn=Hr._2,Vt=Wu,ft=$t.fromRight(ft));break;case i:We--,jr===null&&(Nr=$t.fromRight(ft),ae=new z(b,new z(k,Hr._2,Nr),ae,Xt),(hr===Xt||We>0)&&(Vt=ue,ft=Hr._3(Nr)));break;case k:ae=new z(b,new z(nt,ft,jr),ae,hr),Vt=ue,hr&&hr!==Xt&&We===0?ft=Hr._1.killed($t.fromLeft(hr))(Hr._2):jr?ft=Hr._1.failed($t.fromLeft(jr))(Hr._2):ft=Hr._1.completed($t.fromRight(ft))(Hr._2),jr=null,We++;break;case G:We++,ae=new z(b,new z(nt,ft,jr),ae,hr),Vt=ue,ft=Hr._1;break;case nt:We--,Vt=lu,ft=Hr._1,jr=Hr._2;break}break;case dn:for(var Le in oa)oa.hasOwnProperty(Le)&&(Vo=Vo&&oa[Le].rethrow,qr(oa[Le].handler(ft)));oa=null,hr&&jr?setTimeout(function(){throw $t.fromLeft(jr)},0):$t.isLeft(ft)&&Vo&&setTimeout(function(){if(Vo)throw $t.fromLeft(ft)},0);return;case be:Vt=ue;break;case Ec:return}}function Re(Gt){return function(){if(Vt===dn)return Vo=Vo&&Gt.rethrow,Gt.handler(ft)(),function(){};var Xt=Gf++;return oa=oa||{},oa[Xt]=Gt,function(){oa!==null&&delete oa[Xt]}}}function ur(Gt,Xt){return function(){if(Vt===dn)return Xt($t.right(void 0))(),function(){};var Nr=Re({rethrow:!1,handler:function(){return Xt($t.right(void 0))}})();switch(Vt){case be:hr=$t.left(Gt),Vt=dn,ft=hr,Go(Yr);break;case Ec:hr===null&&(hr=$t.left(Gt)),We===0&&(Vt===Ec&&(ae=new z(b,new z(G,ft(Gt)),ae,hr)),Vt=lu,ft=null,jr=null,Go(++Yr));break;default:hr===null&&(hr=$t.left(Gt)),We===0&&(Vt=lu,ft=null,jr=null)}return Nr}}function Fr(Gt){return function(){var Xt=Re({rethrow:!1,handler:Gt})();return Vt===be&&Go(Yr),Xt}}return{kill:ur,join:Fr,onComplete:Re,isSuspended:function(){return Vt===be},run:function(){Vt===be&&(On.isDraining()?Go(Yr):On.enqueue(function(){Go(Yr)}))}}}function Sv($t,zr,Ie,Yr){var Vt=0,ft={},jr=0,hr={},Ce=new Error("[ParAff] Early exit"),yn=null,ae=t;function We(Re,ur,Fr){var Gt=ur,Xt=null,Nr=null,Hr=0,Le={},wn,ap;t:for(;;)switch(wn=null,Gt.tag){case bt:if(Gt._3===t&&(wn=ft[Gt._1],Le[Hr++]=wn.kill(Re,function(Tw){return function(){Hr--,Hr===0&&Fr(Tw)()}})),Xt===null)break t;Gt=Xt._2,Nr===null?Xt=null:(Xt=Nr._1,Nr=Nr._2);break;case v:Gt=Gt._2;break;case D:case A:Xt&&(Nr=new z(b,Xt,Nr)),Xt=Gt,Gt=Gt._1;break}if(Hr===0)Fr($t.right(void 0))();else for(ap=0,wn=Hr;ap<wn;ap++)Le[ap]=Le[ap]();return Le}function Gf(Re,ur,Fr){var Gt,Xt,Nr,Hr,Le,wn;$t.isLeft(Re)?(Gt=Re,Xt=null):(Xt=Re,Gt=null);t:for(;;){if(Nr=null,Hr=null,Le=null,wn=null,yn!==null)return;if(ur===null){Yr(Gt||Xt)();return}if(ur._3!==t)return;switch(ur.tag){case v:Gt===null?(ur._3=$t.right(ur._1($t.fromRight(Xt))),Xt=ur._3):ur._3=Gt;break;case D:if(Nr=ur._1._3,Hr=ur._2._3,Gt){if(ur._3=Gt,Le=!0,wn=jr++,hr[wn]=We(Ce,Gt===Nr?ur._2:ur._1,function(){return function(){delete hr[wn],Le?Le=!1:Fr===null?Gf(Gt,null,null):Gf(Gt,Fr._1,Fr._2)}}),Le){Le=!1;return}}else{if(Nr===t||Hr===t)return;Xt=$t.right($t.fromRight(Nr)($t.fromRight(Hr))),ur._3=Xt}break;case A:if(Nr=ur._1._3,Hr=ur._2._3,Nr===t&&$t.isLeft(Hr)||Hr===t&&$t.isLeft(Nr))return;if(Nr!==t&&$t.isLeft(Nr)&&Hr!==t&&$t.isLeft(Hr))Gt=Xt===Nr?Hr:Nr,Xt=null,ur._3=Gt;else if(ur._3=Xt,Le=!0,wn=jr++,hr[wn]=We(Ce,Xt===Nr?ur._2:ur._1,function(){return function(){delete hr[wn],Le?Le=!1:Fr===null?Gf(Xt,null,null):Gf(Xt,Fr._1,Fr._2)}}),Le){Le=!1;return}break}Fr===null?ur=null:(ur=Fr._1,Fr=Fr._2)}}function oa(Re){return function(ur){return function(){delete ft[Re._1],Re._3=ur,Gf(ur,Re._2._1,Re._2._2)}}}function Vo(){var Re=ue,ur=Ie,Fr=null,Gt=null,Xt,Nr;t:for(;;)switch(Xt=null,Nr=null,Re){case ue:switch(ur.tag){case v:Fr&&(Gt=new z(b,Fr,Gt)),Fr=new z(v,ur._1,t,t),ur=ur._2;break;case D:Fr&&(Gt=new z(b,Fr,Gt)),Fr=new z(D,t,ur._2,t),ur=ur._1;break;case A:Fr&&(Gt=new z(b,Fr,Gt)),Fr=new z(A,t,ur._2,t),ur=ur._1;break;default:Nr=Vt++,Re=lu,Xt=ur,ur=new z(bt,Nr,new z(b,Fr,Gt),t),Xt=Vf($t,zr,Xt),Xt.onComplete({rethrow:!1,handler:oa(ur)})(),ft[Nr]=Xt,zr&&zr.register(Xt)}break;case lu:if(Fr===null)break t;Fr._1===t?(Fr._1=ur,Re=ue,ur=Fr._2,Fr._2=t):(Fr._2=ur,ur=Fr,Gt===null?Fr=null:(Fr=Gt._1,Gt=Gt._2))}for(ae=ur,Nr=0;Nr<Vt;Nr++)ft[Nr].run()}function Go(Re,ur){yn=$t.left(Re);var Fr;for(var Gt in hr)if(hr.hasOwnProperty(Gt)){Fr=hr[Gt];for(Gt in Fr)Fr.hasOwnProperty(Gt)&&Fr[Gt]()}hr=null;var Xt=We(Re,ae,ur);return function(Nr){return new z(a,function(Hr){return function(){for(var Le in Xt)Xt.hasOwnProperty(Le)&&Xt[Le]();return st}})}}return Vo(),function(Re){return new z(a,function(ur){return function(){return Go(Re,ur)}})}}function ro($t,zr,Ie){return new z(a,function(Yr){return function(){return Sv($t,zr,Ie,Yr)}})}return z.EMPTY=t,z.Pure=zt(r),z.Throw=zt(e),z.Catch=zt(n),z.Sync=zt(u),z.Async=zt(a),z.Bind=zt(o),z.Bracket=zt(i),z.Fork=zt(f),z.Seq=zt(m),z.ParMap=zt(v),z.ParApply=zt(D),z.ParAlt=zt(A),z.Fiber=Vf,z.Supervisor=ia,z.Scheduler=On,z.nonCanceler=st,z}(),zx=tu.Pure,yN=tu.Throw;function Vx(t){return function(r){return r.tag===tu.Pure.tag?tu.Pure(t(r._1)):tu.Bind(r,function(e){return tu.Pure(t(e))})}}function Gx(t){return function(r){return tu.Bind(t,r)}}var Jx=tu.Sync;function jx(t){return function(r){return tu.ParMap(t,r)}}function Xx(t){return function(r){return tu.ParApply(t,r)}}function Qx(t){return function(r){return tu.ParAlt(t,r)}}var ll=tu.Async;function Kx(t,r){return function(){return tu.Fiber(t,null,r)}}var gN=function(){function t(e,n){return e===0&&typeof setImmediate<"u"?setImmediate(n):setTimeout(n,e)}function r(e,n){return e===0&&typeof clearImmediate<"u"?clearImmediate(n):clearTimeout(n)}return function(e,n){return tu.Async(function(u){return function(){var a=t(n,u(e()));return function(){return tu.Sync(function(){return e(r(n,a))})}}})}}(),Yx=tu.Seq;function ni(t){return new Error(t)}function o_(t){return function(){throw t}}function As(t){return function(r){return function(){try{return r()}catch(e){return e instanceof Error||Object.prototype.toString.call(e)==="[object Error]"?t(e)():t(new Error(e.toString()))()}}}}var ui=function(t){return t.throwError};var xN={throwError:o_,Monad0:function(){return ao}};var tC={catchError:Jt(As),MonadThrow0:function(){return xN}};var Co=function(t){return t.catchError};var ks=function(t){var r=Co(t),e=t.MonadThrow0().Monad0(),n=S(e.Bind1().Apply0().Functor0()),u=M(e.Applicative0());return function(a){return r(n(Ct.create)(a))(function(o){return u(xt.create(o))})}};var $o=function(t){return t.state};var ne={liftEffect:K(Z),Monad0:function(){return ao}},de=function(t){return t.liftEffect};var $N=S(Qf);var Ss=function(t){return t};var xs=function(t){return t};var Cs=function(t){return function(r){return t(r)}},v_=function(t){var r=S(t);return{map:function(e){return Cs(r($N(e)))}}};var lA=function(t){return{Applicative0:function(){return s_(t)},Bind1:function(){return pA(t)}}},pA=function(t){var r=lt(t.Bind1()),e=M(t.Applicative0());return{bind:function(n){return function(u){return r(n)(jn(function(a){return e(xt.create(a))})(function(a){var o=u(a);return o}))}},Apply0:function(){return rC(t)}}},rC=function(t){var r=v_(t.Bind1().Apply0().Functor0());return{apply:Gn(lA(t)),Functor0:function(){return r}}},s_=function(t){return{pure:function(){var r=M(t.Applicative0());return function(e){return Ss(r(Ct.create(e)))}}(),Apply0:function(){return rC(t)}}};var eC=function(t){var r=lA(t);return{throwError:function(){var e=M(t.Applicative0());return function(n){return Ss(e(xt.create(n)))}}(),Monad0:function(){return r}}};var _A=function(t){var r=tt(t);return function(e){var n=e.Bind1(),u=lt(n),a=M(e.Applicative0()),o=v_(n.Apply0().Functor0());return{alt:function(i){return function(f){return u(i)(function(m){if(m instanceof Ct)return a(new Ct(m.value0));if(m instanceof xt)return u(f)(function(v){if(v instanceof Ct)return a(new Ct(v.value0));if(v instanceof xt)return a(new xt(r(m.value0)(v.value0)));throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): "+[v.constructor.name])});throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): "+[m.constructor.name])})}},Functor0:function(){return o}}}};var ai=function(t){return t.sequential},Qu=function(t){return t.parallel};var PN=K(Z),IN=function(t){var r=ai(t),e=Dn(t.Applicative1()),n=Qu(t);return function(u){var a=e(u);return function(o){var i=a(function(f){return n(o(f))});return function(f){return r(i(f))}}}},uC=function(t){var r=ai(t),e=t.Applicative1(),n=Qu(t);return function(u){var a=en(u)(e);return function(o){var i=a(function(f){return n(o(f))});return function(f){return r(i(f))}}}},aC=function(t){var r=IN(t);return function(e){return r(e)(PN)}};var oC=kh()();var RN=function(t){return t};var cC=function(t){return t};var vl=function(t){return t.toDuration};var lC={fromDuration:oC(RN)(function(t){return t*1e3}),toDuration:oC(cC)(function(t){return t/1e3})};var pC=function(t,r,e){var n=0,u;return function(a){if(n===2)return u;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+a+")",r,a);return n=1,u=e(),n=2,u}},NN=M(Wt),_C=Pr(L),vC=S(L);var UN=function(t){return t};var ml={map:jx},To={map:Vx};var BN=function(){var t=function(n){if(n instanceof Ct)return n.value0;if(n instanceof xt)return Fu("unsafeFromRight: Left");throw new Error("Failed pattern match at Effect.Aff (line 412, column 21 - line 414, column 54): "+[n.constructor.name])},r=function(n){if(n instanceof xt)return n.value0;if(n instanceof Ct)return Fu("unsafeFromLeft: Right");throw new Error("Failed pattern match at Effect.Aff (line 407, column 20 - line 409, column 55): "+[n.constructor.name])},e=function(n){if(n instanceof xt)return!0;if(n instanceof Ct)return!1;throw new Error("Failed pattern match at Effect.Aff (line 402, column 12 - line 404, column 21): "+[n.constructor.name])};return{isLeft:e,fromLeft:r,fromRight:t,left:xt.create,right:Ct.create}}(),WN=function(t){return Kx(BN,t)},Ca=function(t){return function(){var e=WN(t)();return e.run(),e}},ii=function(t){return _C(Ca(t))};var D_={apply:Xx,Functor0:function(){return ml}};var mA={Applicative0:function(){return ru},Bind1:function(){return qn}},qn={bind:Gx,Apply0:function(){return DA(0)}},ru={pure:zx,Apply0:function(){return DA(0)}},DA=pC("applyAff","Effect.Aff",function(){return{apply:Gn(mA),Functor0:function(){return To}}}),sC=DA(73),mC=M(ru),qN=lt(qn);var Ge={liftEffect:Jx,Monad0:function(){return mA}},sA=de(Ge),DC=function(t){return UN(w(sA(t)))},bC=function(t){return ll(function(r){return vC(DC)(t.join(r))})};var bA=function(t){return function(r){return qN(sA(r.isSuspended))(function(e){return e?sA(_C(r.kill(t,w(NN(void 0))))):ll(function(n){return vC(DC)(r.kill(t,n))})})}};var Ku={parallel:j,sequential:Yx,Monad0:function(){return mA},Applicative1:function(){return HN(0)}},HN=pC("applicativeParAff","Effect.Aff",function(){return{pure:function(){var t=Qu(Ku);return function(r){return t(mC(r))}}(),Apply0:function(){return D_}}});var zN=aC(Ku)(Ht);var VN={append:function(t){return function(r){return function(e){return zN([t(e),r(e)])}}}};var GN=w(mC(void 0)),AC={mempty:GN,Semigroup0:function(){return VN}};var kC={alt:Qx,Functor0:function(){return ml}};var dC=S(ct),JN=ht(qt),jN=M(X);var Dl=Ex;var XN=function(t){return function(r){var e=ti(t)(r),n=dC(function(a){return a})(e),u=tl(dC(function(a){return a})(n));return u}},yC=function(t){return function(r){return Li(Dl)(function(e){return Li(XN(JN(e.value1)(jN(t))))(function(n){return r(new U(e.value0,n))})})}};var Fs=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}();var AA=function(t){var r=!1,e;function n(u){if(u.value0 instanceof wr&&u.value1 instanceof wr)return r=!0,P.value;if(u.value0 instanceof wr){t=new Fs(bb(u.value1),wr.value);return}if(u.value0 instanceof Mt)return r=!0,new F(new U(u.value0.value0,new Fs(u.value0.value1,u.value1)));throw new Error("Failed pattern match at Data.CatQueue (line 82, column 1 - line 82, column 63): "+[u.constructor.name])}for(;!r;)e=n(t);return e},kA=function(t){return function(r){return new Fs(t.value0,new Mt(r,t.value1))}};var hC=function(t){return t.value0 instanceof wr&&t.value1 instanceof wr};var dA=function(){return new Fs(wr.value,wr.value)}();var bl=function(){function t(){}return t.value=new t,t}(),Es=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}();var xC=function(t){return function(r){if(t instanceof bl)return r;if(r instanceof bl)return t;if(t instanceof Es)return new Es(t.value0,kA(t.value1)(r));throw new Error("Failed pattern match at Data.CatList (line 108, column 1 - line 108, column 54): "+[t.constructor.name,r.constructor.name])}},YN=function(t){return function(r){return function(e){var n=function(a){return function(o){return function(i){var f=a,m=o,v=!1,D;function A(b,_,k){if(k instanceof wr)return v=!0,_;if(k instanceof Mt){f=b,m=b(_)(k.value0),i=k.value1;return}throw new Error("Failed pattern match at Data.CatList (line 124, column 3 - line 124, column 59): "+[b.constructor.name,_.constructor.name,k.constructor.name])}for(;!v;)D=A(f,m,i);return D}}},u=function(a){return function(o){var i=a,f=!1,m;function v(D,A){var b=AA(D);if(b instanceof P)return f=!0,n(function(_){return function(k){return k(_)}})(r)(A);if(b instanceof F){i=b.value0.value1,o=new Mt(t(b.value0.value0),A);return}throw new Error("Failed pattern match at Data.CatList (line 120, column 14 - line 122, column 67): "+[b.constructor.name])}for(;!f;)m=v(i,o);return m}};return u(e)(wr.value)}}},CC=function(t){if(t instanceof bl)return P.value;if(t instanceof Es)return new F(new U(t.value0,function(){var r=hC(t.value1);return r?bl.value:YN(xC)(bl.value)(t.value1)}()));throw new Error("Failed pattern match at Data.CatList (line 99, column 1 - line 99, column 61): "+[t.constructor.name])};var $C=function(){return bl.value}(),TC=xC;var FC={append:TC};var EC=function(t){return function(r){return TC(t)(new Es(r,dA))}};var tU=function(t,r,e){var n=0,u;return function(a){if(n===2)return u;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+a+")",r,a);return n=1,u=e(),n=2,u}},rU=tt(FC);var yA=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),Ms=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),b_=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),eU=function(t){var r=!1,e;function n(u){var a=function(f){return f},o=function(f){return function(m){return new yA(f.value0,rU(f.value1)(m))}};if(u.value0 instanceof Ms){var i=CC(u.value1);if(i instanceof P)return r=!0,new Ms(u.value0.value0);if(i instanceof F){t=o(a(i.value0.value0)(u.value0.value0))(i.value0.value1);return}throw new Error("Failed pattern match at Control.Monad.Free (line 227, column 7 - line 231, column 64): "+[i.constructor.name])}if(u.value0 instanceof b_)return r=!0,new b_(u.value0.value0,function(f){return o(u.value0.value1(f))(u.value1)});throw new Error("Failed pattern match at Control.Monad.Free (line 225, column 3 - line 233, column 56): "+[u.value0.constructor.name])}for(;!r;)e=n(t);return e};var nU=function(t){return function(r){return function(e){var n=eU(e);if(n instanceof Ms)return r(n.value0);if(n instanceof b_)return t(n.value0)(n.value1);throw new Error("Failed pattern match at Control.Monad.Free (line 213, column 17 - line 215, column 20): "+[n.constructor.name])}}},MC=function(t){var r=S(t);return nU(function(e){return function(n){return new xt(r(n)(e))}})(Ct.create)},gA=function(t){return new yA(t,$C)},OC=function(t){return gA(new b_(t,j))};var uU={Applicative0:function(){return hA},Bind1:function(){return Os}},aU={map:function(t){return function(r){return cn(Os)(function(){var e=M(hA);return function(n){return e(t(n))}}())(r)}}},Os={bind:function(t){return function(r){return new yA(t.value0,EC(t.value1)(r))}},Apply0:function(){return wC(0)}},hA={pure:function(t){return gA(Ms.create(t))},Apply0:function(){return wC(0)}},wC=tU("freeApply","Control.Monad.Free",function(){return{apply:Gn(uU),Functor0:function(){return aU}}});var iU=M(hA);var Hn=function(t){return gA(new b_(t,function(r){return iU(r)}))};var IC=t=>r=>e=>()=>{if(e.units[r.id]){let n=e.units[r.parent].main;e.units[r.id].main&&e.units[r.id].main.parentNode||e.units[r.id].startBeacon&&e.units[r.id].startBeacon.parentNode||(r.ez?(()=>(e.units[r.id].main?n.appendChild(e.units[r.id].main):(n.appendChild(e.units[r.id].startBeacon),n.appendChild(e.units[r.id].endBeacon)),!0))():t(r.pos)(a=>()=>t(r.dynFamily)(o=>()=>{for(var i=0,f=0,m;f<n.childNodes.length;){if(n.childNodes[f].nodeType===8&&n.childNodes[f].nodeValue==="%-%"+o){f+=1;break}f++}let v=A=>{let b=n.childNodes[A];e.units[r.id].startBeacon?(n.insertBefore(e.units[r.id].startBeacon,b),n.insertBefore(e.units[r.id].endBeacon,b)):n.insertBefore(e.units[r.id].main,b)};for(;f<n.childNodes.length;){var D;if((D=n.childNodes[f].$dekuId)&&t(e.units[D].dynFamily)(b=>()=>t(e.units[D].pos)(k=>()=>o===b&&a<=k?(v(f),!0):!1)())())return!0;if(i===a||n.childNodes[f].nodeType===8&&n.childNodes[f].nodeValue==="%-%"+o+"%-%")return v(f),!0;n.childNodes[f].nodeType===8&&n.childNodes[f].nodeValue.substring(0,3)==="%-%"&&!m&&(m=n.childNodes[f].nodeValue+"%-%"),m||i++,n.childNodes[f].nodeType===8&&n.childNodes[f].nodeValue===m&&(m=void 0,i++),f++}return!1})())())||(r.parent.indexOf("@!%")!==-1?t(r.dynFamily)(o=>()=>(e.units[r.id].main?e.units[o].endBeacon.parentNode.insertBefore(e.units[r.id].main,e.units[o].endBeacon):(e.units[o].endBeacon.parentNode.insertBefore(e.units[r.id].endBeacon,e.units[o].endBeacon),e.units[o].endBeacon.parentNode.insertBefore(e.units[r.id].startBeacon,e.units[r.id].endBeacon)),!0))()||(e.units[r.id].main?n.parentNode.replaceChild(e.units[r.id].main,n):(n.parentNode.replaceChild(e.units[r.id].endBeacon,n),e.units[r.id].endBeacon.parentNode.insertBefore(e.units[r.id].startBeacon,e.units[r.id].endBeacon))):t(r.dynFamily)(o=>()=>(e.units[r.id].startBeacon?(n.insertBefore(e.units[r.id].startBeacon,e.units[o].endBeacon),n.insertBefore(e.units[r.id].endBeacon,e.units[o].endBeacon)):n.insertBefore(e.units[r.id].main,e.units[o].endBeacon),!0))()||(e.units[r.id].startBeacon?(n.appendChild(e.units[r.id].startBeacon),n.appendChild(e.units[r.id].endBeacon)):n.appendChild(e.units[r.id].main)))}};var RC=t=>r=>e=>n=>()=>{var u,a,o=e.id;if(n.scopes[e.scope]||(n.scopes[e.scope]=[]),n.scopes[e.scope].push(o),!t(e.parent)(()=>()=>n.hydrating&&r&&(u=n.allBeacons[e.id])&&(a=n.allBeacons[`${e.id}%-%`])?(n.units[o]={listeners:{},parent:e.parent,scope:e.scope,pos:e.pos,dynFamily:e.dynFamily,startBeacon:u,endBeacon:a},u.$dekuId=o,a.$dekuId=o,!0):!1)()){let f=document.createComment(`%-%${e.id}`),m=document.createComment(`%-%${e.id}%-%`);n.units[o]={listeners:{},parent:e.parent,dynFamily:e.dynFamily,scope:e.scope,pos:e.pos,startBeacon:f,endBeacon:m},f.$dekuId=o,m.$dekuId=o}},oU=new Set(["animate","animateMotion","animateTransform","circle","clipPath","defs","desc","discard","ellipse","feBlend","feColorMatrix","feComponentTransfer","feComposite","feConvolveMatrix","feDiffuseLighting","feDisplacementMap","feDistantLight","feDropShadow","feFlood","feFuncA","feFuncB","feFuncG","feFuncR","feGaussianBlur","feImage","feMerge","feMergeNode","feMorphology","feOffset","fePointLight","feSpecularLighting","feSpotLight","feTile","feTurbulence","filter","foreignObject","g","image","line","linearGradient","marker","mask","metadata","mpath","path","pattern","polygon","polyline","radialGradient","rect","set","stop","svg","switch","symbol","text","textPath","title","tspan","use","view"]);var LC=t=>r=>()=>r.units[t]&&r.units[t].dynFamily?r.units[t].dynFamily:(()=>{throw new Error(`No positional information for ${t}`)})(),NC=t=>r=>()=>r.units[t]&&r.units[t].main&&r.units[t].main.parentNode&&r.units[t].main.parentNode.$dekuId?r.units[t].main.parentNode.$dekuId:r.units[t]&&r.units[t].startBeacon&&r.units[t].startBeacon.parentNode&&r.units[t].startBeacon.parentNode.$dekuId?r.units[t].startBeacon.parentNode.$dekuId:(()=>{throw new Error(`No parent information for ${t}`)})(),SA=t=>r=>()=>r.units[t]&&r.units[t].scope?r.units[t].scope:(()=>{throw new Error(`No scope information for ${t}`)})(),UC=t=>r=>e=>n=>()=>{var u,a=e.id;if(n.scopes[e.scope]||(n.scopes[e.scope]=[]),n.scopes[e.scope].push(a),!t(e.parent)(()=>()=>n.hydrating&&r&&(u=document.documentElement.querySelector(`[data-deku-ssr="${a}"]`))?(n.units[a]={listeners:{},pos:e.pos,parent:e.parent,scope:e.scope,dynFamily:e.dynFamily,main:u},u.$dekuId=a,!0):!1)()){let i=oU.has(e.tag)?document.createElementNS("http://www.w3.org/2000/svg",e.tag):document.createElement(e.tag);n.units[a]={listeners:{},parent:e.parent,pos:e.pos,scope:e.scope,dynFamily:e.dynFamily,main:i},i.$dekuId=a}},BC=t=>r=>e=>n=>u=>()=>{var a=n.id,o;if(u.scopes[n.scope]||(u.scopes[n.scope]=[]),u.scopes[n.scope].push(a),!t(n.parent)(f=>()=>{if(u.hydrating&&r&&(o=document.documentElement.querySelector(`[data-deku-ssr="${f}"]`))){for(var m=0;m<o.childNodes.length;m++){let A=a.split("@-@");if(o.childNodes[m].nodeType===8&&o.childNodes[m].nodeValue===A[0]){var v=m===0||o.childNodes[m-1].nodeType!==3;v&&m!==0?o.insertBefore(document.createTextNode(""),o.childNodes[m]):v?o.prepend(document.createTextNode("")):m=m-1;break}}let D=o.childNodes[m];return u.units[a]={main:D,pos:n.pos,parent:n.parent,scope:n.scope},D.$dekuId=a,!0}return!1})()){let f=document.createTextNode("");u.units[a]={main:f,parent:n.parent,scope:n.scope,pos:n.pos,dynFamily:n.dynFamily},f.$dekuId=a}};function xA(){return{units:{},scopes:{},allBeacons:{}}}var WC=t=>r=>e=>()=>{if(e.units[r.id]){var n=r.id,u=r.value;e.hydrating&&t&&!e.units[n]&&(dom=document.documentElement.querySelector(`[data-deku-ssr="${n}"]`))&&(e.units[n]={listeners:{},parent:r.parent,scope:r.scope,main:dom},e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(n)),e.units[n].main.tagName==="INPUT"&&r.key==="value"||e.units[n].main.tagName==="TEXTAREA"&&r.key==="value"?e.units[n].main.value=u:e.units[n].main.tagName==="INPUT"&&r.key==="checked"?e.units[n].main.checked=u==="true":r.key==="disabled"?e.units[n].main.disabled=u==="true":e.units[n].main.setAttribute(r.key,u)}},qC=t=>r=>e=>()=>{if(e.units[r.id]){var n=r.id,u=r.value;if(e.hydrating&&t&&!e.units[n]&&(dom=document.documentElement.querySelector(`[data-deku-ssr="${n}"]`))&&(e.units[n]={listeners:{},parent:r.parent,scope:r.scope,main:dom},e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(n)),r.key==="@self@")u(e.units[n].main)();else{e.units[n].listeners[r.key]&&e.units[n].main.removeEventListener(r.key,e.units[n].listeners[r.key]);var a=o=>u(o)();e.units[n].main.addEventListener(r.key,a),e.units[n].listeners[r.key]=a}}},HC=t=>r=>e=>()=>{if(e.units[r.id]){var n=r.id;e.hydrating&&t&&!e.units[n]&&(dom=document.documentElement.querySelector(`[data-deku-ssr="${n}"]`))&&(e.units[n]={listeners:{},parent:r.parent,scope:r.scope,main:dom},e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(n)),e.units[n].main.removeAttribute(r.key)}},zC=t=>r=>()=>{if(r.units[t.id]){var e=t.id;r.units[e].main.nodeValue=t.text}},VC=t=>r=>e=>n=>u=>()=>{var a,o,i=n.id,f=n.html,m=n.verb,v=n.cache,D=n.parent,A=n.scope,b=n.pxScope;let _=t(n.parent)(()=>()=>u.hydrating&&r&&(a=document.documentElement.querySelector(`[data-deku-ssr="${i}"]`))?(u.units[i]={listeners:{},pos:n.pos,scope:A,parent:D,main:a},a.$dekuId=i,!0):!1)();if(!_){let G=Object.entries(v);for(var k=0;k<G.length;k++){let nt=G[k][0];G[k][1]===!0?f=f.replace(m+nt+m,'data-deku-attr-internal="'+nt+'"'):f=f.replace(m+nt+m,'<span style="display:contents;" data-deku-elt-internal="'+nt+'"></span>')}o=document.createElement("div"),o.innerHTML=f.trim(),u.units[i]={listeners:{},pos:n.pos,scope:A,parent:D,main:o.firstChild},o.firstChild.$dekuId=i}u.scopes[A]||(u.scopes[A]=[]),u.scopes[A].push(i),o||(o=a),o.querySelectorAll("[data-deku-attr-internal]").forEach(function(G){var nt=G.getAttribute("data-deku-attr-internal");let bt=nt+"@!%"+b;u.units[bt]={listeners:{},main:G,scope:A},u.scopes[A].push(bt)}),o.querySelectorAll("[data-deku-elt-internal]").forEach(function(G){var nt=G.getAttribute("data-deku-elt-internal");let bt=nt+"@!%"+b;u.units[nt+"@!%"+b]={listeners:{},main:G,scope:A},u.scopes[A].push(bt)}),_||u.units[i].main.remove()},GC=t=>r=>()=>{var e=t.id;r.units[e]={main:t.root},t.root.$dekuId=e},CA=t=>r=>e=>n=>()=>{let u=(_,k,G)=>{if(n.units[_].startBeacon){var nt=n.units[_].startBeacon,bt=nt.nextSibling;for(n.units[k].main.insertBefore(nt,G),nt=bt;nt&&nt!==n.units[_].endBeacon;)bt=nt.nextSibling,n.units[k].main.insertBefore(nt,G),nt=bt}else n.units[k].main.insertBefore(n.units[_].main,G)},a=[];a.push(e);for(var o=0;o<a.length;o++){let _=a[o],k=_.id,G=_.parent;n.units[k].containingScope=_.scope;var i=void 0;r(_.pos)(bt=>()=>(i=bt,!0))(),i===void 0&&(i=Number.MAX_VALUE);let nt=n.units[G].main.childNodes;for(var f=0,m=!1,v=0;f<nt.length;){var D;if(D=nt[f].$dekuId){if(r(_.dynFamily)(vr=>()=>m?!1:n.units[D].endBeacon===nt[f]&&vr===D?(n.units[k].pos=t(v),u(k,G,nt[f]),!0):!1)()){m=!0;break}if(n.units[D].dynFamily!==n.units[k].dynFamily){f++;continue}if(m){f++;continue}v===i?(u(k,G,nt[f]),v++,m=!0):n.units[D].endBeacon!==nt[f]&&(n.units[D].pos=t(v),v++)}f++}if(m)return;if(n.units[k].main)n.units[G].main.appendChild(n.units[k].main);else{var A=n.units[k].startBeacon,b=A.nextSibling;for(n.units[G].main.appendChild(A),A=b;A&&A!==n.units[k].endBeacon;)b=A.nextSibling,n.units[G].main.appendChild(A),A=b}}},JC=t=>r=>()=>{if(r.units[t.id]){var e=t.id;if(r.units[e].containingScope&&!t.scopeEq(r.units[e].containingScope)(t.scope))return;if(r.units[e].main)r.units[e].main.remove();else{let a=document.createElement("div");var n=r.units[e].startBeacon,u=n.nextSibling;for(a.appendChild(n),n=u;n&&n!==r.units[e].endBeacon;)u=n.nextSibling,a.appendChild(n),n=u;n===r.units[e].endBeacon&&a.appendChild(n)}}},jC=t=>r=>()=>r.units[t]!==void 0,$A=t=>r=>()=>{r.units[t.id]&&delete r.units[t.id]},XC=$A;function QC(t,r){return r.includes(t)}var Vut=typeof Array.from=="function",Gut=typeof Symbol<"u"&&Symbol!=null&&typeof Symbol.iterator<"u"&&typeof String.prototype[Symbol.iterator]=="function",Jut=typeof String.prototype.fromCodePoint=="function",jut=typeof String.prototype.codePointAt=="function";var ZC=function(t){return function(r){return function(e){return(e|0)===e?t(e):r}}},Br=function(t){return t};var TA=function(t){return function(r){return Math.pow(t,r)|0}};var ws=isFinite;var A_=Math.floor;var vf=function(t){return function(r){return Math.pow(t,r)}},k_=function(t){return function(r){return t%r}},Ps=Math.round;var Is=Math.sin;var sf=3.141592653589793;var r$=Rn(vp),e$=Ln(vp);var Al=function(){return ZC(F.create)(P.value)}(),n$=function(t){if(!ws(t))return 0;if(t>=Br(r$))return r$;if(t<=Br(e$))return e$;if(Jr)return Sn(0)(Al(t));throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): "+[t.constructor.name])},u$=function(t){return n$(Ps(t))};var d_=function(t){return n$(A_(t))};var m$=function(t){return function(r){return QC(t,r)}};var Ui=Math.random;var y_=function(t){return function(r){return function(){var n=Ui(),u=(Br(r)-Br(t)+1)*n+Br(t);return d_(u)}}};var IU=hu(Ga),RU=xn();var D$=function(t){return t};var LU=1,Us=2147483647,NU=function(){return Us-1|0}(),Df=function(t){var r=function(e){return function(n){return function(u){var a=n-e|0,o=IU(u)(a),i=o<e;return i?o+n|0:o}}};return r(LU)(NU)(t)};var UU=0,BU=48271,b$=function(t){return function(r){return RU(Al(k_(Br(BU)*Br(r)+Br(t))(Br(Us))))}},A$=b$(UU);var qs=function(t){var r=S(t);return{map:function(e){return function(n){return function(u){return r(function(a){return new U(e(a.value0),a.value1)})(n(u))}}}}};var LA=function(t){return{Applicative0:function(){return Vs(t)},Bind1:function(){return Hs(t)}}},Hs=function(t){var r=lt(t.Bind1());return{bind:function(e){return function(n){return function(u){return r(e(u))(function(a){var o=n(a.value0);return o(a.value1)})}}},Apply0:function(){return zs(t)}}},zs=function(t){var r=qs(t.Bind1().Apply0().Functor0());return{apply:Gn(LA(t)),Functor0:function(){return r}}},Vs=function(t){var r=M(t.Applicative0());return{pure:function(e){return function(n){return r(new U(e,n))}},Apply0:function(){return zs(t)}}};var NA=function(t){var r=M(t.Applicative0()),e=LA(t);return{state:function(n){return function(u){return r(n(u))}},Monad0:function(){return e}}};var g$=function(t){return function(r){var e=t(r);return e.value0}};var iB=NA(_a),oB=$o(iB);var fB=Hs(_a);var cB=qs(Va);var lB=Ye(Jn);var pB=Er(Jn);var _B=ZS();var vB=function(t){return t};var x$=function(){var t=function(r){return new U(D$(r.newSeed),function(){var e={};for(var n in r)({}).hasOwnProperty.call(r,n)&&(e[n]=r[n]);return e.newSeed=A$(r.newSeed),e}())};return oB(t)}();var x_=cB,S_=S(x_),C$=S_(function(t){return Br(t)/Br(Us)})(x$);var bf=function(t){return g$(vB(t))};var Gs=fB,sB=lt(Gs);var Js=zs(_a),mB=it(Js),S$=function(t){return function(r){var e=Br(r),n=Br(t),u=function(i){return n+k_(i)(e-n+1)},a=S_(Br)(x$),o=mB(S_(pB)(a))(S_(lB(2))(a));return S_(function(i){return d_(u(i))})(o)}},BA=function(t){return function(r){var e=t<=r;return e?S$(t)(r):S$(r)(t)}};var C_=Vs(_a),DB=M(C_);var WA=function(t){return sB(BA(0)(YS(t)-1|0))(function(r){return DB(_B(t)(r))})};var sc=function(t){return t.arbitrary};var $$={arbitrary:C$};var js=function(){return{arbitrary:BA(-1e6)(1e6)}}();var T$=S(ct),gB=S(Qe),hB=S(L),SB=Pr(Kn);var xB=Et(gu),CB=sc(js),$B=Er(pa);var Qs=M(Wt),TB=Ut(Kf(ch(uo))),FB=x(h),F$=tn(),EB=ie(),MB=yt(go),OB=qu(Os),eu=M(X);var Ou=function(t){return t},E$={map:function(t){return function(r){return T$(gB(hB(t)))(r)}}};var mc=function(t){return function(r){return t instanceof F?r(t.value0):Qs(!1)}},wB=function(t){return function(r){return function(){var n=SA(t.id)(r)(),u=NC(t.id)(r)(),a=LC(t.id)(r)(),o={scope:n,parent:u,dynFamily:a,id:t.id,pos:new F(t.pos),ez:!1,raiseId:TB,ctor:tl(FB)};return F$(CA(F.create)(mc)(o))(r)()}}},PB=function(t){return function(r){return function(e){return Zn({doLogic:function(n){return function(u){return function(a){return u.sendToPos({id:a,pos:n})}}},ids:function(n){return function(u){return u.ids}(EB(n))},disconnectElement:function(n){return function(u){return n.disconnectElement({id:u.id,scope:u.scope,parent:u.parent,scopeEq:MB})}},toElt:function(n){return n}})(t)(r)(F$(e))}}},IB=function(t){return function(r){return function(e){return function(n){return OB(Hn(eu(function(u){var a=function(){var i=T$(function(f){return function(m){return Qs(f)}})(PB({dynFamily:n.dynFamily,ez:n.ez,parent:new F(n.parent),pos:n.pos,raiseId:n.raiseId,scope:n.scope})(t)(n.ctor));return Qs(OC(i))}(),o=Qs(Hn(eu(CA(r)(e)(n))));return function(){var f=jC(n.id)(u)();if(f){var m=SA(n.id)(u)();if(m instanceof lf)return o();if(m instanceof Eu&&n.scope instanceof Eu){var v=m$(m.value0)(n.scope.value0);return v?o():a()}return a()}return a()}})))}}}},M$=function(t){var r={ids:function(){var n=Ze(t)(),u=xB(bf(CB)({newSeed:Df(n),size:5}));return SB(ja($B(1))(t))(),u},makeElement:function(){var e=UC(mc)(!1);return function(n){return Hn(Ou(eu(e(n))))}}(),makeDynBeacon:function(){var e=RC(mc)(!1);return function(n){return Hn(Ou(eu(e(n))))}}(),attributeParent:function(){var e=IC(mc);return function(n){return Hn(Ou(eu(e(n))))}}(),makeRoot:function(e){return Hn(Ou(eu(GC(e))))},makeText:function(){var e=BC(mc)(!1)(gt(void 0));return function(n){return Hn(Ou(eu(e(n))))}}(),makePursx:function(){var e=VC(mc)(!1)(gt(void 0));return function(n){return Hn(Ou(eu(e(n))))}}(),setProp:function(){var e=WC(!1);return function(n){return Hn(Ou(eu(e(n))))}}(),setCb:function(){var e=qC(!1);return function(n){return Hn(Ou(eu(e(n))))}}(),unsetAttribute:function(){var e=HC(!1);return function(n){return Hn(Ou(eu(e(n))))}}(),setText:function(e){return Hn(Ou(eu(zC(e))))},sendToPos:function(e){return Hn(Ou(eu(wB(e))))},removeDynBeacon:function(e){return Hn(Ou(eu(XC(e))))},deleteFromCache:function(e){return Hn(Ou(eu($A(e))))},giveNewParent:function(e){return IB(r)(F.create)(mc)(e)},disconnectElement:function(e){return Hn(Ou(eu(JC(e))))}};return r};var Fo=function(){return window};function w$(t,r,e,n){if(typeof window<"u"){var u=window[e];if(u!=null&&n instanceof u)return r(n)}for(var a=n;a!=null;){var o=Object.getPrototypeOf(a),i=o.constructor.name;if(i===e)return r(n);if(i==="Object")return t;a=o}return t}var kt=function(t){return function(r){return w$(P.value,F.create,t,r)}};function P$(t,r,e){return t==null?r:e(t)}var Be=function(t){return P$(t,P.value,F.create)};var HA=kt("HTMLCanvasElement");function L$(t){return t.body}var jB=S(L);var N$=function(t){return jB(Be)(function(){return L$(t)})};var U$=j;function Dc(t){return function(){return t.valueAsNumber}}var yl=kt("HTMLInputElement");function VA(t){return function(){return t.document}}function Ks(t){return function(r){return function(){return r.requestAnimationFrame(t)}}}var GA=j;var q$=ka(me),H$=S(ct),x2=MC(E$),z$=io(uo),C2=Ut(FS(Kf(z$))),$2=M(Wt),JA=lt(Qn),T2=oe(L),F2=Aa(ba),E2=Ut(io(z$)),M2=S(Ae),O2=Pr(L);var w2=function(t){var r=function(u){var a=H$(e(u));return function(o){return q$(a(o))}},e=function(u){return function(a){return function(o){if(o instanceof xt)return q$(H$(n(u))(o.value0));if(o instanceof Ct)return C2;throw new Error("Failed pattern match at Deku.Toplevel (line 47, column 21 - line 49, column 22): "+[o.constructor.name])}(x2(a))}},n=function(u){return function(a){return r(u+1|0)(Cn(function(o){return function(){return void 0,JA(a(t))(o)(),$2(void 0)}}))}};return r(0)},P2=function(t){return function(r){return function(){var n=xA(),u=T2(F2(Fe(0)))(function(){var a=Nx(t)(r);return function(o){return a(M$(o))}}())();return we(w2(n)(u))(function(a){return a(n)})()}}},I2=function(t){return function(){var e=JA(JA(Fo)(VA))(N$)();return gt(E2)(function(n){return P2(n)(t)})(M2(U$)(e))()}},V$=function(t){return O2(I2(t))};var jA=x(h),L2=cf(Ht),G$=M(X),N2=M(Ee),U2={reflectType:function(){return"~"}},J$=ht(qt),B2=S(ct),W2=Ju(Wn),q2=ie(),H2=yt(go),z2=tn();var N={pursxToElement:function(t){return function(r){return function(e){return{cache:wi,element:new _r(function(n){return function(u){return jA}})}}}}},XA=function(t){return t.pursxToElement};var QA=function(t){var r=function(e){return e instanceof _r?e.value0:function(n){return function(u){return jA}}};return r(t)},Yt=function(t){var r=pc(t);return function(e){var n=pc(e);return function(){return function(){return function(u){var a=XA(u);return function(o){return function(i){return function(f){var m=function(D){return function(A){return mr(function(b,_){var k=A.ids(),G=A.ids();D.raiseId(k)();var nt=a(G)(d.value)(f),bt=QA(nt.element),vr=b(L2([G$(A.makePursx({id:k,parent:D.parent,cache:nt.cache,dynFamily:D.dynFamily,pos:D.pos,pxScope:G,scope:D.scope,html:r(i),verb:n(o)})),bt(D)(A),gt(jA)(function(Gr){return G$(A.attributeParent({id:k,parent:Gr,pos:D.pos,dynFamily:D.dynFamily,ez:!1}))})(D.parent)]),_);return function(){return _(A.deleteFromCache({id:k})),vr()}})}},v=new _r(m);return v}}}}}}}},Dt=function(t){var r=Yt(t)(U2)()();return function(){return function(){return function(e){return r(e)(d.value)}}}};var Je=function(){return function(t){var r=XA(t);return function(e){var n=pc(e);return function(u){var a=ko(u)();return{pursxToElement:function(o){return function(i){return function(f){var m=r(o)(d.value)(f);return{cache:So(n(d.value))(!0)(m.cache),element:new _r(function(v){return function(D){return J$(B2(W2(as)(function(A){if(A.value instanceof jp)return D.setProp({id:n(d.value)+("@!%"+o),key:A.key,value:A.value.value0});if(A.value instanceof Xp)return D.setCb({id:n(d.value)+("@!%"+o),key:A.key,value:A.value.value0});if(A.value instanceof us)return D.unsetAttribute({id:n(d.value)+("@!%"+o),key:A.key});throw new Error("Failed pattern match at Deku.Pursx (line 2487, column 52 - line 2504, column 38): "+[A.value.constructor.name])}))(a(d.value)(f)))(function(){var A=QA(m.element);return A}()(v)(D))}})}}}}}}}}};var V2=function(t){return function(r){return function(e){return Zn({doLogic:function(n){return function(u){return function(a){return u.sendToPos({id:a,pos:n})}}},ids:function(n){return function(u){return u.ids}(q2(n))},disconnectElement:function(n){return function(u){return n.disconnectElement({id:u.id,scope:u.scope,parent:u.parent,scopeEq:H2})}},toElt:function(n){return n}})(t)(r)(z2(e))}}},q=function(){return function(t){var r=XA(t);return function(e){var n=pc(e);return function(u){var a=ko(u)();return{pursxToElement:function(o){return function(i){return function(f){var m=r(o)(d.value)(f);return{cache:So(n(d.value))(!1)(m.cache),element:new _r(function(v){return function(D){return J$(V2({parent:new F(n(d.value)+("@!%"+o)),scope:v.scope,raiseId:function(A){return N2(void 0)},pos:v.pos,ez:!1,dynFamily:P.value})(D)(function(A){return A}(a(d.value)(f))))(function(){var A=QA(m.element);return A}()(v)(D))}})}}}}}}}}};var G2=IS()(),J2=K(Yb),j2=fa(a_),X2=i_()(),Sr=function(){return function(){return{defaults:Jt(G2)}}},Q2=function(t){return t.defaults},xr={convertRecordOptions:function(t){return function(r){return function(e){return J2}}}},j$=function(t){return t.convertRecordOptions},zn=function(t){return t.convertOptionsWithDefaults},Cr=function(){return function(t){var r=j$(t);return{convertOptions:function(e){return function(n){return bx(r(e)(d.value)(n))}}}}},K2=function(t){return t.convertOptions},Zt=function(t){var r=K2(t);return function(e){var n=Q2(e);return{convertOptionsWithDefaults:function(u){return function(a){var o=n(a),i=r(u);return function(f){return o(i(f))}}}}}},Y2=function(t){return t.convertOption},dt=function(t){var r=j$(t);return function(e){var n=Y2(e);return function(){return function(){return function(){return function(u){var a=X2(u),o=ko(u)();return{convertRecordOptions:function(i){return function(f){return function(m){return j2(a(d.value)(n(i)(d.value)(o(d.value)(m))))(r(i)(d.value)(m))}}}}}}}}}};var t3=function(){return function(){return function(){return function(t){return function(r){return function(e){return Cv(e.type)(t)?Au(e.type)(t)(e.value):r(e)}}}}}},r3=t3()()();var Eo=function(){return function(t){var r=fe(t);return function(e){return function(n){return{type:r(e),value:n}}}}};var e3=function(t){return Fu("Data.Variant: pattern match failure ["+(t.type+"]"))},KA=function(){return function(){return function(){return function(t){return r3(t)(e3)}}}};var YA=function(){var t=Hv(Db);return function(r){return zv(t(r))}}();var Yu=void 0;var nm=function(t){return t.toInt},t0=function(t){var r=nm(t);return function(e){return r(Yu)}};var Zu={toInt:function(t){return 8}},r0={Nat0:function(){return Zu}},Bi={toInt:function(t){return 7}},e0={Nat0:function(){return Bi}},Wi={toInt:function(t){return 6}},n0={Nat0:function(){return Wi}},uu={toInt:function(t){return 5}},u0={Nat0:function(){return uu}},Ta={toInt:function(t){return 4}},fi={Nat0:function(){return Ta}},Fa={toInt:function(t){return 3}},Mo={Nat0:function(){return Fa}},Ea={toInt:function(t){return 2}},Oo={Nat0:function(){return Ea}},Ma={toInt:function(t){return 1}},wo={Nat0:function(){return Ma}},vn={toInt:function(t){return 0}};var Se=function(t){return function(){return function(r){var e=r.Nat1();return function(){return function(n){return{Nat0:function(){return e},Pos1:function(){return t}}}}}}};var ci={Nat0:function(){return Bi},Nat1:function(){return Zu}};var li={Nat0:function(){return Wi},Nat1:function(){return Zu}};var pi={Nat0:function(){return uu},Nat1:function(){return Zu}};var _i={Nat0:function(){return Ta},Nat1:function(){return Zu}};var qi={Nat0:function(){return Ta},Nat1:function(){return uu}};var vi={Nat0:function(){return Fa},Nat1:function(){return Zu}};var Hi={Nat0:function(){return Fa},Nat1:function(){return uu}};var si={Nat0:function(){return Ea},Nat1:function(){return Zu}};var zi={Nat0:function(){return Ea},Nat1:function(){return uu}};var mi={Nat0:function(){return Ma},Nat1:function(){return Zu}};var Vi={Nat0:function(){return Ma},Nat1:function(){return uu}};var Di={Nat0:function(){return vn},Nat1:function(){return Zu}};var Gi={Nat0:function(){return vn},Nat1:function(){return uu}};var a0={Nat0:function(){return vn},Nat1:function(){return Zu}};var ZA={Nat0:function(){return vn},Nat1:function(){return Bi}};var tk={Nat0:function(){return vn},Nat1:function(){return Wi}};var rk={Nat0:function(){return vn},Nat1:function(){return uu}};var Po={Nat0:function(){return vn},Nat1:function(){return Ta}};var ta={Nat0:function(){return vn},Nat1:function(){return Fa}};var ra={Nat0:function(){return vn},Nat1:function(){return Ea}};var ea={Nat0:function(){return vn},Nat1:function(){return Ma}},Io={Nat0:function(){return vn},Nat1:function(){return vn}};var d3=zc(),y3=S(ir);var i0=xi;var um=function(t){return t};var ek=function(t){var r=nm(t);return function(){return function(e){return function(n){return d3(e)(r(n))}}}};var nk=function(t){var r=t0(t);return function(e){var n=r(d.value),u=function(){return n===0?[]:Ke(0)(n-1|0)}();return y3(e)(u)}};var Oa=[];var Pe=function(t){return function(r){return function(e){return tf(r)(e)}}};var Af={first:function(t){return function(r){return new U(t(r.value0),r.value1)}},second:S(lo),Profunctor0:function(){return Wn}},xl=function(t){return t.second},am=function(t){return t.first};var c0=tn();var w3=function(t){return function(r){return function(e){var n=Pi(e);return function(u){return n(t)(r)(u)}}}};var l0=function(){return function(){return function(t){return w3(c0)(c0)(t)}}};var I3=l0()(),p0=function(){return function(){return function(t){return I3(t)}}};var L3=function(t){return function(r){var e=Pi(r.Profunctor0()),n=am(r);return function(u){return e(t)(function(a){return a.value1(a.value0)})(n(u))}}},_0=function(t){return function(r){return function(e){return L3(function(n){return new U(t(n),function(u){return r(n)(u)})})(e)}}};var v0=function(t){var r=ko(t)(),e=PS(t)()();return function(){return function(){return function(n){return function(u){return _0(r(n))(Jt(e(n)))(u)}}}}};var s0=function(t){return t};var H3=JSON.parse;var z3=JSON.stringify;var j3=ie();var ak=function(t){return j3(xs(t))};function T_(t){return Object.prototype.toString.call(t).slice(8,-1)}var b0=Array.isArray||function(t){return Object.prototype.toString.call(t)==="[object Array]"};var fk=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}();var A0=j;var ck=function(t){var r=ui(eC(t));return function(e){return r(YA(e))}};var lk=function(t){var r=M(s_(t)),e=ck(t);return function(n){return function(u){if(T_(u)===n)return r(A0(u));if(Jr)return e(new fk(n,T_(u)));throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): "+[n.constructor.name,u.constructor.name])}}};var pk=function(t){return lk(t)("String")};var cW=v0({reflectSymbol:function(){return"o"}})()();var lW={reflectSymbol:function(){return"2x"}};var pW={reflectSymbol:function(){return"on"}},_W={reflectSymbol:function(){return"off"}};var vW=p0()()(Wn);var wa=Eo(),sW=M(X),mW=po(),DW=wa({reflectSymbol:function(){return"onOff"}});var im=function(){function t(){}return t.value=new t,t}(),om=function(){function t(){}return t.value=new t,t}(),d0=function(){function t(){}return t.value=new t,t}(),y0=function(){function t(){}return t.value=new t,t}(),vk=function(){function t(){}return t.value=new t,t}(),g0=function(){function t(){}return t.value=new t,t}(),h0=function(){function t(){}return t.value=new t,t}();var S0=function(t){return t},x0=function(t){return t};var C0=function(t){return t};var $0=function(t){return t};var T0=function(t){return t};var F0=function(t){return t},E0=function(t){return t},M0=function(t){return t},O0=function(t){return t},w0=function(t){return t};var sk=function(){function t(){}return t.value=new t,t}(),P0=function(){function t(){}return t.value=new t,t}(),I0=function(){function t(){}return t.value=new t,t}(),mk=function(){function t(){}return t.value=new t,t}(),R0=function(){function t(){}return t.value=new t,t}();var fm=function(t){return t};var Cl=function(t){return t};var bW=function(t){return t},F_=function(t){return t};var kc={toAudioOnOff:K(Z)};var dc=function(t){return t.toAudioParameter},L0=function(t){return t.toAudioOnOff},N0=function(){return Yc.create}(),U0=function(){return Zc.value}();var cm=function(){return s0(function(){var t=cW(d.value)(Af);return function(r){return vW(t(r))}}())},B0=j;var AW=function(){var t=wa({reflectSymbol:function(){return"unit"}})(d.value);return function(r){return F_(t(r))}}();var kW=function(t){return{toAudioParameter:function(r){return AW(r)}}},W0=function(t){return{toAudioParameter:function(){var r=dc(kW(t));return function(e){return r(bW(function(n){return{u:n}}(e)))}}()}},q0=function(){return wa(lW)(d.value)(void 0)}(),H0=function(){var t=wa({reflectSymbol:function(){return"sudden"}})(d.value);return function(r){return F_(t(r))}}();var z0={toAudioParameter:H0},lm={toAudioParameter:function(t){return H0({n:t})}},Dk=function(){return wa({reflectSymbol:function(){return"step"}})(d.value)(void 0)}();var bk=function(){return wa(pW)(d.value)(void 0)}(),E_={x:bk,o:0},Q=function(){return sW(mW(DW(d.value)(E_)))};var V0=function(){return wa(_W)(d.value)(void 0)}();var dW=function(){var t=wa({reflectSymbol:function(){return"numeric"}})(d.value);return function(r){return F_(t(r))}}();var Pa={toAudioParameter:dW};var Ji=function(){return wa({reflectSymbol:function(){return"linear"}})(d.value)(void 0)}();var G0=function(){return wa({reflectSymbol:function(){return"exponential"}})(d.value)(void 0)}(),yW=function(){var t=wa({reflectSymbol:function(){return"envelope"}})(d.value);return function(r){return F_(t(r))}}();var En={toAudioParameter:yW},gW=function(){var t=wa({reflectSymbol:function(){return"cancel"}})(d.value);return function(r){return F_(t(r))}}();var J0={toAudioParameter:gW};var hW=Eo(),SW=hW({reflectSymbol:function(){return"realImg"}}),ce=K(Z),j0=tx(lc),X0=tn();var kf=Cr(),df=dt(xr),yf=Sr()(),Q0={reflectSymbol:function(){return"buffer"}},pm={reflectSymbol:function(){return"frequency"}},xW=function(){function t(){}return t.value=new t,t}(),CW=function(){function t(){}return t.value=new t,t}(),$W=function(){function t(){}return t.value=new t,t}(),TW=function(){function t(){}return t.value=new t,t}(),FW=function(){function t(){}return t.value=new t,t}(),EW=function(){function t(){}return t.value=new t,t}(),MW=function(){function t(){}return t.value=new t,t}(),OW=function(){function t(){}return t.value=new t,t}(),wW=function(){function t(){}return t.value=new t,t}(),PW=function(){function t(){}return t.value=new t,t}(),IW=function(){function t(){}return t.value=new t,t}(),RW=function(){function t(){}return t.value=new t,t}(),LW=function(){function t(){}return t.value=new t,t}(),NW=function(){function t(){}return t.value=new t,t}(),$l=function(t){return{toPeriodicOscSpec:function(r){return SW(d.value)({real:um(r.value0),img:um(r.value1)})}}};var _m={toInitializeTriangleOsc:function(t){return w0(function(r){return{frequency:r}}(t))}};var K0={toInitializeStereoPanner:function(t){return O0(function(r){return{pan:r}}(t))}};var Tl={toInitializeSquareOsc:function(t){return M0(function(r){return{frequency:r}}(t))}};var gf={toInitializeSinOsc:function(t){return E0(function(r){return{frequency:r}}(t))}};var Y0={toInitializeSawtoothOsc:function(t){return F0(function(r){return{frequency:r}}(t))}};var Ak={toInitializeRecorder:function(t){return S0(function(r){return{cb:r}}(t))}};var M_={toInitializeMicrophone:function(t){return x0(function(r){return{microphone:r}}(t))}};var Z0=function(t){return function(r){return{toInitializeIIRFilter:function(e){return function(n){return function(u){return{feedforward:j0(X0(e.value0)),feedback:j0(X0(e.value1))}}}}}}};var _t={toInitializeGain:function(t){return T0(function(r){return{gain:r}}(t))}};var tT={toInitializeConvolver:function(t){return C0(function(r){return{buffer:r}}(t))}},vm={toInitializeConstant:function(t){return $0(function(r){return{offset:r}}(t))}};var UW={convertOption:function(t){return function(r){return ce}}},O_={convertOption:function(t){return function(r){return ce}}},rT={convertOption:function(t){return function(r){return ce}}},eT={convertOption:function(t){return function(r){return F.create}}},nT={convertOption:function(t){return function(r){return ce}}},Fl={convertOption:function(t){return function(r){return ce}}},uT={convertOption:function(t){return function(r){return ce}}},aT={convertOption:function(t){return function(r){return ce}}},iT={convertOption:function(t){return function(r){return ce}}},oT={convertOption:function(t){return function(r){return ce}}},fT={convertOption:function(t){return function(r){return ce}}},cT={convertOption:function(t){return function(r){return ce}}},lT={convertOption:function(t){return function(r){return ce}}},pT={convertOption:function(t){return function(r){return ce}}},kk={convertOption:function(t){return function(r){return ce}}},El={convertOption:function(t){return function(r){return ce}}},sm={convertOption:function(t){return function(r){return ce}}},mm={convertOption:function(t){return function(r){return ce}}};var Dm={convertOption:function(t){return function(r){return ce}}},_T={convertOption:function(t){return function(r){return ce}}},vT={convertOption:function(t){return function(r){return ce}}},sT={convertOption:function(t){return function(r){return ce}}},dk={convertOption:function(t){return function(r){return ce}}};var mT={convertOption:function(t){return function(r){return ce}}},yk={convertOption:function(t){return function(r){return ce}}},ji={convertOption:function(t){return function(r){return ce}}},bi={convertOption:function(t){return function(r){return ce}}},DT={convertOption:function(t){return function(r){return ce}}},gk={convertOption:function(t){return function(r){return ce}}},BW=function(t){return t.toPeriodicOscSpec},Ml=function(t){var r=BW(t);return{convertOption:function(e){return function(n){return r}}}},hk=function(t){return t.toInitializeWaveShaper},bT=function(t){return t.toInitializeTriangleOsc},AT=function(t){return t.toInitializeStereoPanner},kT=function(t){return t.toInitializeSquareOsc},dT=function(t){return t.toInitializeSinOsc},yT=function(t){return t.toInitializeSawtoothOsc},gT=function(t){return t.toInitializeRecorder},Sk=function(t){return t.toInitializePlayBuf},hT=function(t){return t.toInitializePeriodicOsc},ST=function(t){return t.toInitializePeaking},xT=function(t){return t.toInitializeNotch},CT=function(t){return t.toInitializeMicrophone},$T=function(t){return t.toInitializeLowshelf},xk=function(t){return t.toInitializeLowpass},Ck=function(t){return t.toInitializeLoopBuf},TT=function(t){return t.toInitializeIIRFilter},FT=function(t){return t.toInitializeHighshelf},$k=function(t){return t.toInitializeHighpass},ET=function(t){return t.toInitializeGain},MT=function(t){return t.toInitializeDynamicsCompressor},Tk=function(t){return t.toInitializeDelay},OT=function(t){return t.toInitializeConvolver},wT=function(t){return t.toInitializeConstant},Fk=function(t){return t.toInitializeBandpass},Ek=function(t){return t.toInitializeAllpass};var WW={oversample:q0},qW=function(t){var r=zn(t);return{toInitializeWaveShaper:function(e){return r(xW.value)(WW)(e)}}},PT={toInitializeWaveShaper:function(){var t=hk(qW(Zt(kf(df(UW)()()()({reflectSymbol:function(){return"curve"}})))(yf)));return function(r){return t(function(e){return{curve:e}}(r))}}()},HW=function(){return{bufferOffset:0,playbackRate:1,duration:P.value}}(),w_=function(t){var r=zn(t);return{toInitializePlayBuf:function(e){return r(CW.value)(HW)(e)}}},Ia={toInitializePlayBuf:function(){var t=Sk(w_(Zt(kf(df(O_)()()()(Q0)))(yf)));return function(r){return t(function(e){return{buffer:e}}(r))}}()},zW={},Ol=function(t){var r=zn(t);return{toInitializePeriodicOsc:function(e){return r($W.value)(zW)(e)}}},VW={q:1,gain:0},IT=function(t){var r=zn(t);return{toInitializePeaking:function(e){return r(TW.value)(VW)(e)}}};var GW={q:1},RT=function(t){var r=zn(t);return{toInitializeNotch:function(e){return r(FW.value)(GW)(e)}}};var JW={gain:0},LT=function(t){var r=zn(t);return{toInitializeLowshelf:function(e){return r(EW.value)(JW)(e)}}};var jW={q:1},Mk=function(t){var r=zn(t);return{toInitializeLowpass:function(e){return r(MW.value)(jW)(e)}}},bm={toInitializeLowpass:function(){var t=xk(Mk(Zt(kf(df(kk)()()()(pm)))(yf)));return function(r){return t(function(e){return{frequency:e}}(r))}}()},XW=function(){return{loopStart:0,loopEnd:0,playbackRate:1,duration:P.value}}(),wl=function(t){var r=zn(t);return{toInitializeLoopBuf:function(e){return r(OW.value)(XW)(e)}}},cr={toInitializeLoopBuf:function(){var t=Ck(wl(Zt(kf(df(El)()()()(Q0)))(yf)));return function(r){return t(function(e){return{buffer:e}}(r))}}()},QW={gain:0},NT=function(t){var r=zn(t);return{toInitializeHighshelf:function(e){return r(wW.value)(QW)(e)}}};var KW={q:1},Ok=function(t){var r=zn(t);return{toInitializeHighpass:function(e){return r(PW.value)(KW)(e)}}},Pl={toInitializeHighpass:function(){var t=$k(Ok(Zt(kf(df(dk)()()()(pm)))(yf)));return function(r){return t(function(e){return{frequency:e}}(r))}}()},YW=function(){return{ratio:12,attack:.003,release:.25,knee:30,threshold:-24}}(),UT=function(t){var r=zn(t);return{toInitializeDynamicsCompressor:function(e){return r(IW.value)(YW)(e)}}},ZW={maxDelayTime:1},wk=function(t){var r=zn(t);return{toInitializeDelay:function(e){return r(RW.value)(ZW)(e)}}},Ai={toInitializeDelay:function(){var t=Tk(wk(Zt(kf(df(yk)()()()({reflectSymbol:function(){return"delayTime"}})))(yf)));return function(r){return t(function(e){return{delayTime:e}}(r))}}()},tq={q:1},ki=function(t){var r=zn(t);return{toInitializeBandpass:function(e){return r(LW.value)(tq)(e)}}},BT={toInitializeBandpass:function(){var t=Fk(ki(Zt(kf(df(bi)()()()(pm)))(yf)));return function(r){return t(function(e){return{frequency:e}}(r))}}()},rq={q:1},Pk=function(t){var r=zn(t);return{toInitializeAllpass:function(e){return r(NW.value)(rq)(e)}}},WT={toInitializeAllpass:function(){var t=Ek(Pk(Zt(kf(df(gk)()()()(pm)))(yf)));return function(r){return t(function(e){return{frequency:e}}(r))}}()};var uq=function(t,r,e){var n=0,u;return function(a){if(n===2)return u;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+a+")",r,a);return n=1,u=e(),n=2,u}},zT=K(Z);var Am=ie(),I_=tn();var qT=M(Wt),Dr=M(X),An=M(Ee),xe=rn(Ht)(h),an=S(ct),je=KA()()(),su=x(h),HT=ps(),P_=Eo(),aq=ht(qt),iq=Pr(Kn),sn=ka(me),oq=nA()()(),VT=Ju(Wn),fq=S(e_),cq=zb({reflectType:function(){return 0}})()()();var lq=function(){function t(){}return t.value=new t,t}();var km={convertOption:function(t){return function(r){return zT}}},dm={convertOption:function(t){return function(r){return zT}}};var pq=function(t){return t.toInitializeAnalyser},ye=function(t){if(t instanceof lf)return P.value;if(t instanceof Eu)return new F(t.value0);throw new Error("Failed pattern match at Ocarina.Control (line 38, column 1 - line 38, column 38): "+[t.constructor.name])},wu=xx({doLogic:Jo,ids:function(t){return function(r){return r.ids}(Am(t))},disconnectElement:function(t){return function(r){return t.disconnectXFromY({from:r.id,to:r.parent})}},toElt:function(t){return t}})({fromElt:I_,connectToParent:function(t){return function(r){return t.connectXToY({from:r.id,to:r.parent})}}});var _q=function(){return{cb:function(t){return qT(qT(void 0))},fftSize:vk.value,maxDecibels:-30,minDecibels:-100,smoothingTimeConstant:.8,channelCount:2,channelCountMode:mk.value,channelInterpretation:sk.value}}(),ym=function(t){var r=zn(t);return{toInitializeAnalyser:function(e){return r(lq.value)(_q)(e)}}};var vq=function(t){var r=CT(t);return function(e){var n=r(e),u=function(a){return function(o){return mr(function(i,f){var m=o.ids();a.raiseId(m)();var v=i(Dr(o.makeMicrophone({id:m,parent:a.parent,scope:ye(a.scope),microphone:n.microphone})),f);return function(){return f(o.deleteFromCache({id:m})),v()}})}};return new _r(u)}},R_=function(t){return vq(t)};var kn=Zn({doLogic:Jo,ids:function(t){return function(r){return r.ids}(Am(t))},disconnectElement:function(t){return function(r){return t.disconnectXFromY({from:r.id,to:r.parent})}},toElt:function(t){return t}}),sq=function(t){var r=pq(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([Dr(f.makeAnalyser({id:D,parent:i.parent,scope:ye(i.scope),cb:a.cb,fftSize:TA(2)(function(){if(a.fftSize instanceof im)return 7;if(a.fftSize instanceof om)return 8;if(a.fftSize instanceof d0)return 9;if(a.fftSize instanceof y0)return 10;if(a.fftSize instanceof vk)return 11;if(a.fftSize instanceof g0)return 12;if(a.fftSize instanceof h0)return 13;throw new Error("Failed pattern match at Ocarina.Control (line 198, column 27 - line 205, column 40): "+[a.fftSize.constructor.name])}()),maxDecibels:a.maxDecibels,minDecibels:a.minDecibels,smoothingTimeConstant:a.smoothingTimeConstant,channelCount:a.channelCount,channelCountMode:function(){if(a.channelCountMode instanceof R0)return"explicit";if(a.channelCountMode instanceof mk)return"max";if(a.channelCountMode instanceof I0)return"clamped-max";throw new Error("Failed pattern match at Ocarina.Control (line 211, column 41 - line 214, column 52): "+[a.channelCountMode.constructor.name])}(),channelInterpretation:function(){if(a.channelInterpretation instanceof sk)return"speakers";if(a.channelInterpretation instanceof P0)return"discrete";throw new Error("Failed pattern match at Ocarina.Control (line 215, column 46 - line 217, column 47): "+[a.channelInterpretation.constructor.name])}()})),an(function(b){return je({cb:function(_){return f.setAnalyserNodeCb({id:D,cb:_})}})(b)})(n),kn({parent:new F(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},gm=function(t){var r=sq(t);return function(e){return r(e)(su)}},GT=function(t){var r=OT(t);return function(e){return function(n){var u=r(e),a=function(o){return function(i){return mr(function(f,m){var v=i.ids();o.raiseId(v)();var D=f(xe([Dr(i.makeConvolver({id:v,parent:o.parent,scope:ye(o.scope),buffer:u.buffer})),kn({parent:new F(v),scope:o.scope,raiseId:function(A){return An(void 0)}})(i)(ze(n))]),m);return function(){return m(i.deleteFromCache({id:v})),D()}})}};return new _r(a)}}},mq=function(){return function(){return function(t){var r=TT(t);return function(e){return function(n){return function(u){return function(a){var o=r(u)(e)(n),i=function(f){return function(m){return mr(function(v,D){var A=m.ids();f.raiseId(A)();var b=v(xe([Dr(m.makeIIRFilter({id:A,parent:f.parent,scope:ye(f.scope),feedforward:HT(o.feedforward),feedback:HT(o.feedback)})),kn({parent:new F(A),scope:f.scope,raiseId:function(_){return An(void 0)}})(m)(ze(a))]),D);return function(){return D(m.deleteFromCache({id:A})),b()}})}};return new _r(i)}}}}}}},Dq=mq()(),JT=function(){return function(){return function(t){return Dq(t)(d.value)(d.value)}}},Ik=function(t){var r=gT(t);return function(e){return function(n){var u=r(e),a=function(o){return function(i){return mr(function(f,m){var v=i.ids();o.raiseId(v)();var D=f(xe([Dr(i.makeRecorder({id:v,parent:o.parent,scope:ye(o.scope),cb:u.cb})),kn({parent:new F(v),scope:o.scope,raiseId:function(A){return An(void 0)}})(i)(n)]),m);return function(){return m(i.deleteFromCache({id:v})),D()}})}};return new _r(a)}}},bq=function(t){return function(r){return mr(function(e,n){var u=r.ids();return n(r.makeSpeaker({id:u})),e(kn({parent:new F(u),scope:new Eu("toplevel"),raiseId:function(a){return An(void 0)}})(r)(ze(t)),n)})}},yc=bq,Pt=function(t){return function(r){return function(e){return mu(t)(r)(su)(e)}}},mu=function(t){var r=ET(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([Dr(f.makeGain({id:D,parent:i.parent,scope:ye(i.scope),gain:a.gain})),sn(an(function(b){return je({gain:jT(639)(i.scope)(f)(function(_){return f.setGain(function(k){return{id:D,gain:k}}(_))})})(b)})(n)),kn({parent:new F(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},jT=uq("tmpResolveAU","Ocarina.Control",function(){var t=function(){var o=P_({reflectSymbol:function(){return"unit"}})(d.value);return function(i){return Cl(o(i))}}(),r=function(){var o=P_({reflectSymbol:function(){return"sudden"}})(d.value);return function(i){return Cl(o(i))}}(),e=function(){var o=P_({reflectSymbol:function(){return"numeric"}})(d.value);return function(i){return Cl(o(i))}}(),n=function(){var o=P_({reflectSymbol:function(){return"envelope"}})(d.value);return function(i){return Cl(o(i))}}(),u=function(){var o=P_({reflectSymbol:function(){return"cancel"}})(d.value);return function(i){return Cl(o(i))}}(),a=function(o){return function(i){return function(f){return function(m){return je({numeric:function(v){return Dr(f(e(v)))},envelope:function(v){return Dr(f(n(v)))},cancel:function(v){return Dr(f(u(v)))},sudden:function(v){return Dr(f(r(v)))},unit:function(v){var D=Pt(_t)(1)([v.u]);return mr(function(A,b){var _=Fe(P.value)();return A(aq(kn({parent:P.value,scope:o,raiseId:function(k){return iq(Cu(new F(k))(_))}})(i)(D))(mr(function(k,G){return function(){var bt=Ze(_)();if(bt instanceof P)return void 0;if(bt instanceof F)return G(f(t({i:bt.value0})));throw new Error("Failed pattern match at Ocarina.Control (line 1827, column 42 - line 1829, column 80): "+[bt.constructor.name])}(),An(void 0)})),b)})}})(m)}}}};return a}),Kr=jT(1804),Aq=Pt(_t),kq=function(t){var r=Ck(t);return function(e){return function(n){var u=r(e),a=function(o){return function(i){return mr(function(f,m){var v=i.ids();o.raiseId(v)();var D=f(xe([Dr(i.makeLoopBuf({id:v,parent:o.parent,scope:ye(o.scope),buffer:u.buffer,playbackRate:u.playbackRate,loopStart:u.loopStart,loopEnd:u.loopEnd,duration:u.duration})),sn(an(function(A){return je({buffer:function(b){return Dr(i.setBuffer({id:v,buffer:b}))},playbackRate:Kr(o.scope)(i)(function(b){return i.setPlaybackRate(function(_){return{id:v,playbackRate:_}}(b))}),loopStart:function(b){return Dr(i.setLoopStart({id:v,loopStart:b}))},loopEnd:function(b){return Dr(i.setLoopEnd({id:v,loopEnd:b}))},onOff:function(b){return Dr(i.setOnOff({id:v,onOff:b}))}})(A)})(n))]),m);return function(){return m(i.deleteFromCache({id:v})),D()}})}};return new _r(a)}}},tr=function(t){return kq(t)};var dq=function(t){var r=hT(t);return function(e){return function(n){var u=r(e),a=function(o){return function(i){return mr(function(f,m){var v=i.ids();o.raiseId(v)();var D=f(xe([Dr(i.makePeriodicOsc({id:v,parent:o.parent,scope:ye(o.scope),frequency:u.frequency,spec:u.spec})),sn(an(function(A){return je({frequency:Kr(o.scope)(i)(function(b){return i.setFrequency(function(_){return{id:v,frequency:_}}(b))}),onOff:function(b){return Dr(i.setOnOff({id:v,onOff:b}))},spec:function(b){return Dr(i.setPeriodicOsc({id:v,spec:b}))}})(A)})(n))]),m);return function(){return m(i.deleteFromCache({id:v})),D()}})}};return new _r(a)}}},Il=function(t){return dq(t)};var yq=function(t){var r=Sk(t);return function(e){return function(n){var u=r(e),a=function(o){return function(i){return mr(function(f,m){var v=i.ids();o.raiseId(v)();var D=f(xe([Dr(i.makePlayBuf({id:v,parent:o.parent,scope:ye(o.scope),buffer:u.buffer,playbackRate:u.playbackRate,bufferOffset:u.bufferOffset,duration:u.duration})),sn(an(function(A){return je({buffer:function(b){return Dr(i.setBuffer({id:v,buffer:b}))},playbackRate:Kr(o.scope)(i)(function(b){return i.setPlaybackRate(function(_){return{id:v,playbackRate:_}}(b))}),bufferOffset:function(b){return Dr(i.setBufferOffset({id:v,bufferOffset:b}))},onOff:function(b){return Dr(i.setOnOff({id:v,onOff:b}))},duration:function(b){return Dr(i.setDuration({id:v,duration:b}))}})(A)})(n))]),m);return function(){return m(i.deleteFromCache({id:v})),D()}})}};return new _r(a)}}},au=function(t){return yq(t)};var gq=function(t){var r=yT(t);return function(e){return function(n){var u=r(e),a=function(o){return function(i){return mr(function(f,m){var v=i.ids();o.raiseId(v)();var D=f(xe([Dr(i.makeSawtoothOsc({id:v,parent:o.parent,scope:ye(o.scope),frequency:u.frequency})),sn(an(function(A){return je({frequency:Kr(o.scope)(i)(function(b){return i.setFrequency(function(_){return{id:v,frequency:_}}(b))}),onOff:function(b){return Dr(i.setOnOff({id:v,onOff:b}))}})(A)})(n))]),m);return function(){return m(i.deleteFromCache({id:v})),D()}})}};return new _r(a)}}},XT=function(t){return gq(t)};var hq=function(t){var r=dT(t);return function(e){return function(n){var u=r(e),a=function(o){return function(i){return mr(function(f,m){var v=i.ids();o.raiseId(v)();var D=f(xe([Dr(i.makeSinOsc({id:v,parent:o.parent,scope:ye(o.scope),frequency:u.frequency})),sn(an(function(A){return je({frequency:Kr(o.scope)(i)(function(b){return i.setFrequency(function(_){return{id:v,frequency:_}}(b))}),onOff:function(b){return Dr(i.setOnOff({id:v,onOff:b}))}})(A)})(n))]),m);return function(){return m(i.deleteFromCache({id:v})),D()}})}};return new _r(a)}}},gc=function(t){return hq(t)},QT=function(t){var r=gc(t);return function(e){return r(e)(su)}},Sq=function(t){var r=kT(t);return function(e){return function(n){var u=r(e),a=function(o){return function(i){return mr(function(f,m){var v=i.ids();o.raiseId(v)();var D=f(xe([Dr(i.makeSquareOsc({id:v,parent:o.parent,scope:ye(o.scope),frequency:u.frequency})),sn(an(function(A){return je({frequency:Kr(o.scope)(i)(function(b){return i.setFrequency(function(_){return{id:v,frequency:_}}(b))}),onOff:function(b){return Dr(i.setOnOff({id:v,onOff:b}))}})(A)})(n))]),m);return function(){return m(i.deleteFromCache({id:v})),D()}})}};return new _r(a)}}},L_=function(t){return Sq(t)},KT=function(t){var r=L_(t);return function(e){return r(e)(su)}},xq=function(t){var r=bT(t);return function(e){return function(n){var u=r(e),a=function(o){return function(i){return mr(function(f,m){var v=i.ids();o.raiseId(v)();var D=f(xe([Dr(i.makeTriangleOsc({id:v,parent:o.parent,scope:ye(o.scope),frequency:u.frequency})),sn(an(function(A){return je({frequency:Kr(o.scope)(i)(function(b){return i.setFrequency(function(_){return{id:v,frequency:_}}(b))}),onOff:function(b){return Dr(i.setOnOff({id:v,onOff:b}))}})(A)})(n))]),m);return function(){return m(i.deleteFromCache({id:v})),D()}})}};return new _r(a)}}},hm=function(t){return xq(t)};var Cq=function(t){var r=Ek(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([Dr(f.makeAllpass({id:D,parent:i.parent,scope:ye(i.scope),frequency:a.frequency,q:a.q})),sn(an(function(b){return je({frequency:Kr(i.scope)(f)(function(_){return f.setFrequency(function(k){return{id:D,frequency:k}}(_))}),q:Kr(i.scope)(f)(function(_){return f.setQ(function(k){return{id:D,q:k}}(_))})})(b)})(n)),kn({parent:new F(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},Rk=function(t){var r=Cq(t);return function(e){return function(n){return r(e)(su)(n)}}},Lk=function(t){var r=Fk(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([Dr(f.makeBandpass({id:D,parent:i.parent,scope:ye(i.scope),frequency:a.frequency,q:a.q})),sn(an(function(b){return je({frequency:Kr(i.scope)(f)(function(_){return f.setFrequency(function(k){return{id:D,frequency:k}}(_))}),q:Kr(i.scope)(f)(function(_){return f.setQ(function(k){return{id:D,q:k}}(_))})})(b)})(n)),kn({parent:new F(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},Xi=function(t){var r=Lk(t);return function(e){return function(n){return r(e)(su)(n)}}},N_=function(t){var r=Tk(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([Dr(f.makeDelay({id:D,parent:i.parent,scope:ye(i.scope),delayTime:a.delayTime,maxDelayTime:a.maxDelayTime})),sn(an(function(b){return je({delayTime:Kr(i.scope)(f)(function(_){return f.setDelay(function(k){return{id:D,delayTime:k}}(_))})})(b)})(n)),kn({parent:new F(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},hf=function(t){var r=N_(t);return function(e){return function(n){return r(e)(su)(n)}}},$q=function(t){var r=MT(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([Dr(f.makeDynamicsCompressor({id:D,parent:i.parent,scope:ye(i.scope),threshold:a.threshold,ratio:a.ratio,knee:a.knee,attack:a.attack,release:a.release})),sn(an(function(b){return je({threshold:Kr(i.scope)(f)(function(_){return f.setThreshold(function(k){return{id:D,threshold:k}}(_))}),ratio:Kr(i.scope)(f)(function(_){return f.setRatio(function(k){return{id:D,ratio:k}}(_))}),knee:Kr(i.scope)(f)(function(_){return f.setKnee(function(k){return{id:D,knee:k}}(_))}),attack:Kr(i.scope)(f)(function(_){return f.setAttack(function(k){return{id:D,attack:k}}(_))}),release:Kr(i.scope)(f)(function(_){return f.setRelease(function(k){return{id:D,release:k}}(_))})})(b)})(n)),kn({parent:new F(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},YT=function(t){var r=$q(t);return function(e){return r(e)(su)}},Tq=function(){return function(t){return function(r){return oq({doLogic:Jo,ids:function(e){return function(n){return n.ids}(Am(e))},disconnectElement:function(e){return function(n){return e.disconnectXFromY({from:n.id,to:n.parent})}},toElt:function(e){return e}})({fromEltO1:I_,fromEltO2:I_,toElt:I_,wrapElt:function(e){return Aq(1)([e])},giveNewParent:function(e){return function(n){return function(u){return function(a){return e.connectXToY({from:n.id,to:n.parent})}}}},deleteFromCache:function(e){return function(n){return n.deleteFromCache}(Am(e))}})(t)(VT(fq(function(e){return e(void 0)}))(I_(r)))}}},Fq=Tq(),iu=function(t){return function(r){return Fq(Hb(t))(VT(cq(d.value))(r))}};var Nk=function(t){var r=$k(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([Dr(f.makeHighpass({id:D,parent:i.parent,scope:ye(i.scope),frequency:a.frequency,q:a.q})),sn(an(function(b){return je({frequency:Kr(i.scope)(f)(function(_){return f.setFrequency(function(k){return{id:D,frequency:k}}(_))}),q:Kr(i.scope)(f)(function(_){return f.setQ(function(k){return{id:D,q:k}}(_))})})(b)})(n)),kn({parent:new F(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},Rl=function(t){var r=Nk(t);return function(e){return function(n){return r(e)(su)(n)}}},Eq=function(t){var r=FT(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([Dr(f.makeHighshelf({id:D,parent:i.parent,scope:ye(i.scope),frequency:a.frequency,gain:a.gain})),sn(an(function(b){return je({frequency:Kr(i.scope)(f)(function(_){return f.setFrequency(function(k){return{id:D,frequency:k}}(_))}),gain:Kr(i.scope)(f)(function(_){return f.setGain(function(k){return{id:D,gain:k}}(_))})})(b)})(n)),kn({parent:new F(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},ZT=function(t){var r=Eq(t);return function(e){return function(n){return r(e)(su)(n)}}},tF=function(t){var r=xk(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([Dr(f.makeLowpass({id:D,parent:i.parent,scope:ye(i.scope),frequency:a.frequency,q:a.q})),sn(an(function(b){return je({frequency:Kr(i.scope)(f)(function(_){return f.setFrequency(function(k){return{id:D,frequency:k}}(_))}),q:Kr(i.scope)(f)(function(_){return f.setQ(function(k){return{id:D,q:k}}(_))})})(b)})(n)),kn({parent:new F(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},Ll=function(t){var r=tF(t);return function(e){return function(n){return r(e)(su)(n)}}},Mq=function(t){var r=$T(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([Dr(f.makeLowshelf({id:D,parent:i.parent,scope:ye(i.scope),frequency:a.frequency,gain:a.gain})),sn(an(function(b){return je({frequency:Kr(i.scope)(f)(function(_){return f.setFrequency(function(k){return{id:D,frequency:k}}(_))}),gain:Kr(i.scope)(f)(function(_){return f.setGain(function(k){return{id:D,gain:k}}(_))})})(b)})(n)),kn({parent:new F(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},rF=function(t){var r=Mq(t);return function(e){return function(n){return r(e)(su)(n)}}},Oq=function(t){var r=xT(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([Dr(f.makeNotch({id:D,parent:i.parent,scope:ye(i.scope),frequency:a.frequency,q:a.q})),sn(an(function(b){return je({frequency:Kr(i.scope)(f)(function(_){return f.setFrequency(function(k){return{id:D,frequency:k}}(_))}),q:Kr(i.scope)(f)(function(_){return f.setQ(function(k){return{id:D,q:k}}(_))})})(b)})(n)),kn({parent:new F(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},eF=function(t){var r=Oq(t);return function(e){return function(n){return r(e)(su)(n)}}},wq=function(t){var r=AT(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([Dr(f.makeStereoPanner({id:D,parent:i.parent,scope:ye(i.scope),pan:a.pan})),sn(an(function(b){return je({pan:Kr(i.scope)(f)(function(_){return f.setPan(function(k){return{id:D,pan:k}}(_))})})(b)})(n)),kn({parent:new F(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},nF=function(t){var r=wq(t);return function(e){return r(e)(su)}},Pq=function(t){var r=ST(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([Dr(f.makePeaking({id:D,parent:i.parent,scope:ye(i.scope),frequency:a.frequency,q:a.q,gain:a.gain})),sn(an(function(b){return je({frequency:Kr(i.scope)(f)(function(_){return f.setFrequency(function(k){return{id:D,frequency:k}}(_))}),q:Kr(i.scope)(f)(function(_){return f.setQ(function(k){return{id:D,q:k}}(_))}),gain:Kr(i.scope)(f)(function(_){return f.setGain(function(k){return{id:D,gain:k}}(_))})})(b)})(n)),kn({parent:new F(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},uF=function(t){var r=Pq(t);return function(e){return function(n){return r(e)(su)(n)}}},aF=function(t){var r=hk(t);return function(e){return function(n){var u=r(e),a=function(o){return function(i){return mr(function(f,m){var v=i.ids();o.raiseId(v)();var D=f(xe([Dr(i.makeWaveShaper({id:v,parent:o.parent,scope:ye(o.scope),curve:u.curve,oversample:u.oversample})),kn({parent:new F(v),scope:o.scope,raiseId:function(A){return An(void 0)}})(i)(ze(n))]),m);return function(){return m(i.deleteFromCache({id:v})),D()}})}};return new _r(a)}}},Iq=function(t){var r=wT(t);return function(e){return function(n){var u=r(e),a=function(o){return function(i){return mr(function(f,m){var v=i.ids();o.raiseId(v)();var D=f(xe([Dr(i.makeConstant({id:v,parent:o.parent,scope:ye(o.scope),offset:u.offset})),sn(an(function(A){return je({offset:Kr(o.scope)(i)(function(b){return i.setOffset(function(_){return{id:v,offset:_}}(b))}),onOff:function(b){return Dr(i.setOnOff({id:v,onOff:b}))}})(A)})(n))]),m);return function(){return m(i.deleteFromCache({id:v})),D()}})}};return new _r(a)}}},Sm=function(t){return Iq(t)};function Uk(){window.scrollTo(0,0)}var on=O("button");var iF=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),U_=function(){function t(){}return t.value=new t,t}(),hc=function(){function t(){}return t.value=new t,t}(),B_=function(){function t(){}return t.value=new t,t}(),Sc=function(){function t(){}return t.value=new t,t}(),W_=function(){function t(){}return t.value=new t,t}(),q_=function(){function t(){}return t.value=new t,t}(),oF=function(){function t(){}return t.value=new t,t}(),xm=function(){function t(){}return t.value=new t,t}(),Cm=function(){function t(){}return t.value=new t,t}(),H_=function(){function t(){}return t.value=new t,t}(),z_=function(){function t(){}return t.value=new t,t}(),fF=function(){function t(){}return t.value=new t,t}(),Nl=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Bk=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var Lq="numeric",Nq="sudden",Uq="unit",Bq="cancel",Wq="step",qq="linear",Hq="exponential",zq="envelope",cF=function(t,r,e,n){if(e.type===Nq)t.value=e.value.n;else if(e.type===Uq)r.id&&Gq(r.id,n),n.units[e.value.i].main.connect(t),r.id=e.value.i;else if(e.type===Lq)t[e.value.t.type===Wq?"setValueAtTime":e.value.t.type===qq?"linearRampToValueAtTime":e.value.t.type===Hq?"exponentialRampToValueAtTime":"linearRampToValueAtTime"](e.value.n,e.value.o);else if(e.type===Bq)e.value.hold?t.cancelAndHoldAtTime(e.value.o):t.cancelScheduledValues(e.value.o);else if(e.type===zq){let u=e.value.o;t.cancelScheduledValues(Math.max(0,u)),t.setValueCurveAtTime(e.value.p,u,e.value.d)}else throw new Error("No idea what to do with "+JSON.stringify(e))},Vq=function(t,r,e,n,u){return n[e]||(n[e]={}),cF(r.parameters.get(e),n[e],u,t)},Ra=function(t,r,e,n,u){return n[e]||(n[e]={}),cF(r[e],n[e],u,t)},le=function(t,r,e,n){let u=t("@fan@")(a=>a)(e);n.scopes[u]||(n.scopes[u]=[]),n.scopes[u].push(r),n.units[r].scope=u},pe=function(t,r){r.toConnect[t]&&(r.toConnect[t].forEach(function(e){e.w?r.units[e.w]?e.f():(r.toConnect[e.w]||(r.toConnect[e.w]=[]),r.toConnect[e.w].push({f:e.f})):e.f()}),delete r.toConnect[t])},_e=function(t,r,e,n){t()(u=>lF(r,u,n))(e)},lF=function(t,r,e){var n=function(){e.units[t].audioOutgoing.push(r),e.units[t].pendingOn||(e.units[t].main.connect(e.units[r].main),e.units[r].se&&e.units[t].main.connect(e.units[r].se))};if(!e.units[t]){e.toConnect[t]||(e.toConnect[t]=[]);var u={f:n};r!==t&&!e.units[r]&&(u.w=r),e.toConnect[t].push(u);return}if(!e.units[r]){e.toConnect[r]||(e.toConnect[r]=[]);var u={f:n};r!==t&&!e.units[t]&&(u.w=t),e.toConnect[r].push(u);return}n()};function Wk(t){return function(r){return function(){delete r.units[t.id]}}}function qk(t){return function(r){return function(){lF(t.from,t.to,r)}}}var Gq=function(t,r){if(r.units[t].scope==="@fan@")return;let e=r.units[t].scope;r.scopes[e].forEach(n=>{delete r.units[n]}),delete r.scopes[e]};function Hk(t){return function(r){return function(){var e=t.from,n=t.to;if(!r.units[e]||(r.units[e].audioOutgoing=r.units[e].audioOutgoing.filter(function(a){return a!==n}),r.units[e].main.disconnect(r.units[n].main),r.units[n].se&&r.units[e].main.disconnect(r.units[n].se),r.units[e].scope==="@fan@"))return;let u=r.units[e].scope;r.scopes[u].forEach(a=>{delete r.units[a]}),delete r.scopes[u]}}}var zk=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"allpass",Q:r.q,frequency:r.frequency})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},Vk=t=>r=>e=>()=>{var n=r.id,u=r.cb,a=new AnalyserNode(e.context,r),o=u(a)();e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],analyserOrig:u,analyser:o,main:e.context.createGain(),se:a},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},Gk=t=>r=>e=>()=>{var n=r.id,u=r.options;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new AudioWorkletNode(e.context,u.name,{numberOfInputs:u.numberOfInputs,numberOfOutputs:u.numberOfOutputs,outputChannelCount:u.outputChannelCount,parameterData:u.parameterData,processorOptions:u.processorOptions})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},Jk=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"bandpass",Q:r.q,frequency:r.frequency})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},jk=t=>r=>e=>()=>{var n=r.id,u=function(o,i){return new ConstantSourceNode(o,i)},a={offset:r.offset};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:u,onOff:!1,pendingOn:!0,main:u(e.context,a)},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},Xk=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new ConvolverNode(e.context,{buffer:r.buffer})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},Qk=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DelayNode(e.context,{delayTime:r.delayTime,maxDelayTime:r.maxDelayTime})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},Kk=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DynamicsCompressorNode(e.context,{knee:r.knee,ratio:r.ratio,threshold:r.threshold,attack:r.attack,release:r.release})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},Yk=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new GainNode(e.context,{gain:r.gain})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},Zk=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highpass",Q:r.q,frequency:r.frequency})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},td=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highshelf",frequency:r.frequency,gain:r.gain})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},rd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new IIRFilterNode(e.context,{feedforward:r.feedforward,feedback:r.feedback})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},ed=t=>r=>e=>()=>{var n=r.id,u=function(o,i){return new AudioBufferSourceNode(o,i)},a={loop:!0,buffer:r.buffer,loopStart:r.loopStart,loopEnd:r.loopEnd,playbackRate:r.playbackRate};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:u,onOff:!1,pendingOn:!0,main:u(e.context,a)},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},nd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowpass",Q:r.q,frequency:r.frequency})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},ud=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowshelf",frequency:r.frequency,gain:r.gain})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},ad=t=>r=>e=>()=>{var n=r.id,u=r.element,a=function(){var o=e.context.createMediaElementSource(u);return o};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],createClosure:a,resumeClosure:{},main:a()},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},id=t=>r=>e=>()=>{var n=r.id;e.units[r.id]={main:e.context.createMediaStreamSource(r.microphone),controllers:{},audioOutgoing:[],controlOutgoing:[]},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},od=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"notch",frequency:r.frequency,Q:r.q})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},fd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"peaking",frequency:r.frequency,Q:r.q,gain:r.gain})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},cd=t=>r=>e=>()=>{var n=r.id,u=function(o,i){var f={frequency:i.frequency,periodicWave:i.spec.type==="wave"?i.spec.value:Jd(e.context)(i.spec.value.real)(i.spec.value.img)()},m=new OscillatorNode(o,f);return m},a={frequency:r.frequency,type:"custom",spec:r.spec};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:u,onOff:!1,pendingOn:!0,main:u(e.context,a)},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},ld=t=>r=>e=>()=>{var n=r.id,u=function(o,i){var f={loop:i.loop,buffer:i.buffer,playbackRate:i.playbackRate};return new AudioBufferSourceNode(o,f)},a={loop:!1,buffer:r.buffer,playbackRate:r.playbackRate,bufferOffset:r.bufferOffset,duration:t(void 0)(o=>o)(r.duration)};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:u,onOff:!1,pendingOn:!0,main:u(e.context,a)},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},pd=t=>r=>e=>()=>{var n=r.id,u=r.cb,a=e.context.createMediaStreamDestination(),o=new MediaRecorder(a.stream);u(o)(),o.start(),e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],recorderOrig:u,recorder:o,main:e.context.createGain(),se:a},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},_d=t=>r=>e=>()=>{var n=r.id,u=function(o,i){return new OscillatorNode(o,i)},a={frequency:r.frequency,type:"sawtooth"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:u,onOff:!1,pendingOn:!0,main:u(e.context,a)},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},vd=t=>r=>e=>()=>{var n=r.id,u=function(o,i){return new OscillatorNode(o,i)},a={frequency:r.frequency,type:"sine"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:u,onOff:!1,pendingOn:!0,main:u(e.context,a)},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},sd=t=>r=>()=>{r.units[t.id]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:r.context.createGain(),se:r.context.destination}},md=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new StereoPannerNode(e.context,{pan:r.pan})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},Dd=t=>r=>e=>()=>{var n=r.id,u=function(o,i){return new OscillatorNode(o,i)},a={frequency:r.frequency,type:"square"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:u,onOff:!1,pendingOn:!0,main:u(e.context,a)},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},bd=t=>r=>e=>()=>{var n=r.id,u=function(o,i){return new OscillatorNode(o,i)},a={frequency:r.frequency,type:"triangle"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:u,onOff:!1,pendingOn:!0,main:u(e.context,a)},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},Ad=t=>r=>e=>()=>{var n=r.id,u=r.curve,a=r.oversample;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new WaveShaperNode(e.context,{curve:u,oversample:a.type})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)};function kd(t){return function(r){return function(){var e=t.id,n=t.cb;r.units[e].analyserOrig!==n&&(r.units[e].analyser&&r.units[e].analyser(),r.units[e].analyser=n(r.units[e].se)(),r.units[e].analyserOrig=n)}}}function dd(t){return function(r){return function(){var e=t.cb,n=t.id;if(r.units[n].recorderOrig!==e){r.units[n].recorder&&r.units[n].recorder.stop();var u=e;r.units[n].recorderOrig=e;var a=new MediaRecorder(r.units[n].se);u(a)(),a.start()}}}}function yd(t){return function(r){return function(){var e=t.id,n=t.curve;r.units[e].main.curve=n}}}function gd(t){return function(r){return function(){var e=t.id,n=t.paramName,u=t.paramValue;Vq(r,r.units[e].main,n,r.units[e].controllers,u)}}}var La=function(t,r,e){r.resume&&t.value.n!==void 0&&(r.resume[e]=t.value.n)};function hd(t){return function(r){return function(){var e=t.id,n=t.gain;Ra(r,r.units[e].main,"gain",r.units[e].controllers,n),La(n,r.units[e],"gain")}}}function Sd(t){return function(r){return function(){var e=t.id,n=t.q;Ra(r,r.units[e].main,"Q",r.units[e].controllers,n),La(n,r.units[e],"Q")}}}function xd(t){return function(r){return function(){var e=t.id,n=t.buffer;r.units[e].resume&&(r.units[e].resume.buffer=n)}}}function Cd(t){return function(r){return function(){var e=t.id,n=t.buffer;r.units[e].main.buffer=n}}}function $d(t){return function(r){return function(){var e=t.id,n=t.spec;r.units[e].resume&&(r.units[e].resume.spec=n)}}}function Td(t){return function(r){return function(){var e=t.id,n=t.pan;Ra(r,r.units[e].main,"pan",r.units[e].controllers,n),La(n,r.units[e],"pan")}}}function Fd(t){return function(r){return function(){var e=t.id,n=t.threshold;Ra(r,r.units[e].main,"threshold",r.units[e].controllers,n),La(n,r.units[e],"threshold")}}}function Ed(t){return function(r){return function(){var e=t.id,n=t.loopStart;r.units[e].main.loopStart=n,r.units[e].resume.loopStart=n}}}function Md(t){return function(r){return function(){var e=t.id,n=t.loopEnd;r.units[e].main.loopEnd=n,r.units[e].resume.loopEnd=n}}}function Od(t){return function(r){return function(){var e=t.id,n=t.bufferOffset;r.units[e].resume.bufferOffset=n}}}function wd(t){return function(r){return function(e){return function(){var n=r.id,u=r.duration;e.units[n].duration=t(void 0)(a=>a)(u)}}}}function Pd(t){return function(r){return function(){var e=t.id,n=t.release;Ra(r,r.units[e].main,"release",r.units[e].controllers,n),La(n,r.units[e],"release")}}}function Id(t){return function(r){return function(){var e=t.id,n=t.offset;Ra(r,r.units[e].main,"offset",r.units[e].controllers,n),La(n,r.units[e],"offset")}}}function Rd(t){return function(r){return function(){var e=t.id,n=t.ratio;Ra(r,r.units[e].main,"ratio",r.units[e].controllers,n),La(n,r.units[e],"ratio")}}}function Ld(t){return function(r){return function(){var e=t.id,n=t.attack;Ra(r,r.units[e].main,"attack",r.units[e].controllers,n),La(n,r.units[e],"attack")}}}function Nd(t){return function(r){return function(){var e=t.id,n=t.knee;Ra(r,r.units[e].main,"knee",r.units[e].controllers,n),La(n,r.units[e],"knee")}}}function Ud(t){return function(r){return function(){var e=t.id,n=t.delayTime;Ra(r,r.units[e].main,"delayTime",r.units[e].controllers,n),La(n,r.units[e],"delayTime")}}}function Bd(t){return function(r){return function(){var e=t.id,n=t.playbackRate;Ra(r,r.units[e].main,"playbackRate",r.units[e].controllers,n),La(n,r.units[e],"playbackRate")}}}function Wd(t){return function(r){return function(){var e=t.id,n=t.frequency;Ra(r,r.units[e].main,"frequency",r.units[e].controllers,n),La(n,r.units[e],"frequency")}}}function qd(t){return function(r){return function(){var e=t.id,n=t.onOff;n.x.type==="on"?Jq(e)(n)(r)():n.x.type==="off"&&jq(e)(n)(r)()}}}var Jq=function(t){return function(r){return function(e){return function(){if(!e.units[t].onOff){e.units[t].pendingOn=!1,e.units[t].onOff=!0,e.units[t].main=e.units[t].createClosure(e.context,e.units[t].resume);for(var n=0;n<e.units[t].audioOutgoing.length;n++){var u=e.units[t].audioOutgoing[n];e.units[t].main.connect(e.units[u].main),e.units[u].se&&e.units[t].main.connect(e.units[u].se)}e.units[t].resume&&e.units[t].resume.bufferOffset?typeof e.units[t].resume.duration=="number"?e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.bufferOffset,e.units[t].resume.duration):e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.bufferOffset):e.units[t].resume&&e.units[t].resume.loopStart?e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.loopStart):e.units[t].main.start(e.deprecatedWriteHead+r.o)}}}}},jq=function(t){return function(r){return function(e){return function(){if(!!e.units[t].onOff){e.units[t].onOff=!1;var n=e.units[t].main;n.addEventListener("ended",()=>{n.disconnect()}),n.stop(e.deprecatedWriteHead+r.o)}}}}};function Hd(t){for(var r=new Float32Array(t.length),e=0;e<t.length;e++)r[e]=t[e];return r}function $m(t){return function(){t.stop()}}function zd(t){return function(r){return function(e){return function(){var n=[];e.ondataavailable=function(u){n.push(u.data)},e.onstop=function(){var u=new Blob(n,{type:t});r(u)(),n=null}}}}}function Vd(t){return function(r){return function(){return navigator.mediaDevices.getUserMedia({audio:t,video:r})}}}function V_(t){return function(){var r=new Uint8Array(t.frequencyBinCount);return t.getByteFrequencyData(r),r}}function Gd(t){return function(){var r=t.createConstantSource();return r.offset.value=0,r.connect(t.destination),r.start(),function(){r.stop(),r.disconnect(t.destination)}}}var Jd=function(t){return function(r){return function(e){return function(){for(var n=new Float32Array(r.length),u=new Float32Array(e.length),a=0;a<r.length;a++)n[a]=r[a];for(var a=0;a<e.length;a++)u[a]=e[a];return t.createPeriodicWave(n,u,{disableNormalization:!0})}}}};function Sf(t){return function(){return{context:t,deprecatedWriteHead:0,units:{},scopes:{},unsu:{},toConnect:{}}}}function jd(t){return function(){t.close()}}function Xd(t){return function(){return fetch(t).then(function(r){return r.arrayBuffer()},function(r){return console.error("Error fetching buffer",r),Promise.reject(r)})}}function Qd(t){return function(r){return function(){return t.decodeAudioData(r)}}}function Kd(){return new(window.AudioContext||window.webkitAudioContext)}function Yd(t){return function(){return t.state}}function G_(t){return function(){return t.currentTime}}function pF(t){return function(r){return function(e){return function(){t.then(e,r)}}}}var Kq=xv(L),Yq=Ut(AC),Zq=K(Z),t4=ht(_A(mb)(_a)),r4=lk(_a),e4=S(v_(Va)),n4=pk(_a),u4=lt(qn),a4=de(Ge),i4=function(t){return function(r){return ll(function(e){return Kq(Yq)(pF(r)(function(n){return e(xt.create(t(n)))()})(function(n){return e(Ct.create(n))()}))})}};var o4=function(t){return jn(function(r){return ni("Promise failed, couldn't extract JS Error or String")})(Zq)(ak(t4(r4("Error")(t))(e4(ni)(n4(t)))))},_F=i4(o4),Tm=function(t){return u4(a4(t))(_F)};function Zd(t){return function(){return URL.createObjectURL(t)}}var v4=Et(gu);var s4=cn(Qn);var m4=sc(js),D4=Pr(Kn),b4=Er(pa),A4=lt(qn);var k4=Pn(Wt),d4=S(To),vF=M(no);var sF=function(t){return function(r){return function(e){return Jt(zd(t))(e)(function(){var n=s4(r);return function(u){return n(Zd(u))}}())}}};var xc=function(t){return{ids:function(){var e=Ze(t)(),n=v4(bf(m4)({newSeed:Df(e),size:5}));return D4(ja(b4(1))(t))(),n},deleteFromCache:Wk,disconnectXFromY:Hk,connectXToY:qk,makeAllpass:zk(gt),makeAnalyser:Vk(gt),makeAudioWorkletNode:Gk(gt),makeBandpass:Jk(gt),makeConstant:jk(gt),makeConvolver:Xk(gt),makeDelay:Qk(gt),makeDynamicsCompressor:Kk(gt),makeGain:Yk(gt),makeHighpass:Zk(gt),makeHighshelf:td(gt),makeIIRFilter:rd(gt),makeLoopBuf:ed(gt),makeLowpass:nd(gt),makeLowshelf:ud(gt),makeMediaElement:ad(gt),makeMicrophone:id(gt),makeNotch:od(gt),makePeaking:fd(gt),makePeriodicOsc:cd(gt),makePlayBuf:ld(gt),makeRecorder:pd(gt),makeSawtoothOsc:_d(gt),makeSinOsc:vd(gt),makeSpeaker:sd,makeSquareOsc:Dd(gt),makeStereoPanner:md(gt),makeTriangleOsc:bd(gt),makeWaveShaper:Ad(gt),setAnalyserNodeCb:kd,setMediaRecorderCb:dd,setWaveShaperCurve:yd,setAudioWorkletParameter:gd,setBuffer:xd,setConvolverBuffer:Cd,setDuration:wd(gt),setPeriodicOsc:$d,setOnOff:qd,setBufferOffset:Od,setLoopStart:Ed,setLoopEnd:Md,setRatio:Rd,setOffset:Id,setAttack:Ld,setGain:hd,setQ:Sd,setPan:Td,setThreshold:Fd,setRelease:Pd,setKnee:Nd,setDelay:Ud,setPlaybackRate:Bd,setFrequency:Wd}},at=function(t){return function(r){return A4(Tm(Xd(r)))(function(){var e=Qd(t);return function(n){return Tm(e(n))}}())}},J_=function(t){var r=de(t);return function(e){return r(Yd(e))}},y4=J_(ne);var Mn=function(t){return de(t)(Kd)},Na=function(t){var r=de(t);return function(e){return r(Gd(e))}},mn=function(t){var r=de(t);return function(e){return r(function(){var u=y4(e)();return k4(u!=="closed")(jd(e))()})}},g4=j,h4=j,Fm=function(t){return function(r){return d4(function(e){return{microphone:function(){return t?vF(g4(e)):P.value}(),camera:function(){return r?vF(h4(e)):P.value}()}})(Tm(Vd(t)(r)))}};var S4=ai(Ku),x4=ht(kC),mF=Qu(Ku),Em=lt(qn),DF=de(Ge),ry=mt(ke),j_=rr(or),X_=S(ct),ty=M(Wt),C4=Mn(Ge),$4=Na(Ge),T4=mn(ne),F4=it(pn),bF=ht(qt),AF=M(X),E4=mt(bs),Ki=function(){function t(){}return t.value=new t,t}(),Yi=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),ua=function(){function t(){}return t.value=new t,t}(),Xe=Uk,Ro=function(t){return S4(x4(mF(Em(bC(t))(DF)))(mF(bA(ni("We navigated away from the page"))(t))))},Q_=function(t){var r=ht(t);return function(e){var n=M(e);return function(u){return function(a){return r(n(ua.value))(a)}}}},kF=Q_(qt)(X),ou=function(t){var r=ht(t),e=S(t.Functor0());return function(n){var u=M(n);return function(a){return function(o){return r(u(ry(ee.value)(Vr(w(o)))))(e(function(i){return ry(ee.value)(Vr(w(j_(i)(o))))})(e(function(i){return i.value0})(a)))}}}},Mm=function(t){return function(r){return function(e){return function(n){return function(u){return function(a){return X_(function(o){return ry(ee.value)(Vr(w(function(){if(o.value0 instanceof Ki)return ty(void 0);if(o.value0 instanceof Yi)return j_(j_(o.value0.value0)(t(ty(void 0))))(r(ua.value));if(o.value0 instanceof ua)return function(){o.value1(),r(Ki.value)();var f=Ca(Em(C4)(function(m){return Em($4(m))(function(v){return Em(e(m))(function(D){return DF(function(){var b=n(m)(D)(),_=j_(j_(b)(v))(T4(m));return r(new Yi(_))(),_})})})}))();return t(function(){return r(ua.value)(),ii(Ro(f))()})(),void 0};throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 57, column 21 - line 75, column 26): "+[o.value0.constructor.name])}())))})(F4(X_(U.create)(a))(bF(AF(ty(void 0)))(X_(function(o){return o.value0})(u))))}}}}}},fu=function(t){return function(r){return function(e){return function(){return t(e)(),r(new iF(e))()}}}},Om=function(t){return function(r){return function(e){return function(n){return function(u){return Ue(function(a){return function(o){var i=kF(r)(o);return cl(bF(AF(E4(Ot.value)("cursor: pointer;")))(Mm(e)(a)(n)(u)(r)(i)))([Ve(X_(function(f){if(f instanceof ua)return t;if(f instanceof Ki)return"\u23F3";if(f instanceof Yi)return"\u{1F6D1}";throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 126, column 17 - line 129, column 35): "+[f.constructor.name])})(i))])}})}}}}},vt=function(t){return function(r){return function(e){return function(n){return Ue(function(u){return function(a){var o=kF(t)(a);return on(Mm(r)(u)(e)(n)(t)(o))([Ve(X_(function(i){if(i instanceof ua)return"Turn on";if(i instanceof Ki)return"Loading...";if(i instanceof Yi)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 100, column 17 - line 103, column 42): "+[i.constructor.name])})(o))])}})}}}};var dF=Aa(ba),M4=S(ct),O4=Mn(ne),w4=S(L),P4=rr(or),I4=mn(ne),Ul=function(t){return function(r){return function(){var n=Sf(t)(),u=dF(Fe(0))(),a=we(yc([new ri(M4(function(o){return Yp.create(LS(o))})(r))])(xc(u)))(function(o){return o(n)})();return a}}};var Y=function(t){return function(r){return function(){var n=Sf(t)(),u=dF(Fe(0))(),a=we(yc(r)(xc(u)))(function(o){return o(n)})();return a}}},wm=function(t){return function(){var e=O4();return w4(function(n){return P4(n)(I4(e))})(Y(e)(t))()}};var R4=Yt({reflectType:function(){return`<section>
  <h2 id="allpass">Allpass filter</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">all-pass filter</a> <a href="https://en.wikipedia.org/wiki/All-pass_filter">passes through all frequencies of a source at equal volume but changes their phase</a>. Its use by itself is imperceptible, as the human ear (mostly) does not pick up on phase shifts by themselves. However, when an all-pass filter's output is mixed with several chained all-pass filters plus the original source, you hear a neat phaser effect.</p>

  <p>The <code>bangOn</code> is an event that turns the loop buffer on. We'll learn more about turning things on and off in the "Events" section.</p>

  <pre><code>\\buf -> run2_
  [ fan1 (loopBuf buf bangOn)
    \\b _ -> gain_ 0.2
      [ b
      , allpass_ 700.0
          [ allpass_ { frequency: 990.0, q: 20.0 } [ b ]
          , allpass_ 1110.0
              [ b
              , allpass_ { frequency: 2010.0, q: 30.0 } [ b ]
              ]
          ]
      ]
  ]
</code></pre>

  @allpass@
  </section>
`}})({reflectType:function(){return"@"}})()()(q()(N)({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}})),L4=tr(cr),N4=Q(),U4=Pt(_t),yF=Rk(WT),gF=Rk(Pk(Zt(Cr()(dt(dt(xr)(DT)()()()({reflectSymbol:function(){return"q"}}))(gk)()()()({reflectSymbol:function(){return"frequency"}})))(Sr()()))),B4=function(){return d.value}(),hF=function(t){return function(r){return function(e){return R4(d.value)(B4)({allpass:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([iu(L4(u)(N4))(function(a){return U4(.2)([a,yF(700)([gF({frequency:990,q:20})([a]),yF(1110)([a,gF({frequency:2010,q:30})([a])])])])})])}})})}}};function Lo(t){return function(e,n,u){if(n===null)return new t(e);var a=e.byteLength,o=t.BYTES_PER_ELEMENT,i=Math.min(a,n>>>0);if(u===null)return new t(e,i);var f=Math.min((a-i)/o,u);return new t(e,i,f)}}var q4=Lo(Uint8ClampedArray),H4=Lo(Uint32Array),z4=Lo(Uint16Array),SF=Lo(Uint8Array),V4=Lo(Int32Array),G4=Lo(Int16Array),J4=Lo(Int8Array),j4=Lo(Float32Array),X4=Lo(Float64Array);function xF(t){for(var r=t.length,e=new Array(r),n=0;n<r;n++)e[n]=t[n];return e}var Pm={create:SF,BinaryValue0:function(){}};var Im=function(t){return function(r){return function(){return xF(r)}}};var Bl=Yu,Wl=Yu,ql=Yu,Pu=Yu,Iu=Yu,Ru=Yu,Lu=Yu,Nu=Yu;function Rm(t){return t|0}var rH=function(t,r,e){var n=0,u;return function(a){if(n===2)return u;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+a+")",r,a);return n=1,u=e(),n=2,u}},eH=Pr(L),nH=Pn(Wt),xf=Cn(function(t){return function(){var e=Fo(),n=Or(!0)(),u=rH("fx","FRP.Event.Animate",function(){return eH(Jt(Ks)(e)(function(){var i=Ur(n)();return nH(i)(function(){return t(void 0)(),u(19)()})()}))}),a=u(15);return a(),te(!1)(n)}});var FF=nk(Zu),ay=Pe(Se(u0)()(Po)()(rk)),iy=Pe(Se(fi)()(ta)()(Po)),oy=Pe(Se(Mo)()(ra)()(ta)),fy=Pe(Se(Oo)()(ea)()(ra)),cy=Pe(Se(wo)()(Io)()(ea)),uH=Cr(),aH=dt(dt(xr)(dm)()()()({reflectSymbol:function(){return"fftSize"}})),iH={reflectSymbol:function(){return"cb"}},oH=Sr()(),fH=Q(),cH=Ja(Ev),lH=Dt({reflectType:function(){return`<section>
  <h2 id="analyser">Analyser</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode">analyser node</a> provides methods to recuperate the analysed data of an input. This is how, for example, Google Meet shows the little animation around a microphone icon. Ocarina provides the possibility to use the analyser as the terminus of an audio graph <i>or</i> as part of a longer DSP chain, as in the following example. The example uses an FFT size of 256, which is indicated in Ocarina as <code>TTT8</code> (two to the eighth power).</p>

  <pre><code>analyser_ { cb, fftSize: TTT8 } [ loopBuf atar bangOn ]</code></pre>

  ~analyser~
  </section>
`}})()()(q()(N)({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}})),pH=Qv(ya),_H=K(Z),vH=Q_(qt)(X),yr=ht(qt),gr=M(X),sH=mt(ol),mH=Aa(ba),ey=S(ct),DH=$u(Wt)(Me),bH=Im(Pm),ny=Er(pa),AH=en(i0)(Wt),uy=S(L),kH=Pe(Se(r0)()(ZA)()(a0)),dH=Pe(Se(e0)()(tk)()(ZA)),yH=Pe(Se(n0)()(rk)()(tk)),gH=J_(ne),hH=Pn(Wt),SH=mn(ne),xH=nk(uu),$r=mt(Ar),CH="background-color: rgb(150,30,10);",$H="background-color: rgb(130,60,10);",TH="background-color: rgb(80,90,10);",FH="background-color: rgb(10,130,10);",EH="background-color: rgb(10,100,0);",MH=FF(function(t){return ay(CH)(iy($H)(oy(TH)(fy(FH)(cy(EH)(Oa)))))}),OH=function(t){var r=gm(ym(Zt(uH(aH(t)()()()(iH)))(oH)));return function(e){var n=tr(e);return function(u){return function(a){return r({cb:a,fftSize:om.value})([n(u)(fH)])}}}},wH=OH(km)(cr),PH=function(){return d.value}(),Tr="background-color: rgb(255,255,255,0.0);",IH=function(t){var r=S(t);return function(e){var n=ek(e)();return function(u){return function(a){var o=ek(a)();return function(i){return function(f){var m=mt(f);return function(v){return function(D){return function(A){return function(b){return function(_){return r(function(k){var G=n(o(k)(A))(b);return G?m(Ot.value)(n(o(MH)(A))(b)):m(Ot.value)(Tr)})(_)}}}}}}}}}}},K_=IH(ct),Cf=K_(vn)(Gi),RH=Cf(vn)(Di)(Ar)(Gi)(Di),LH=Cf(Ma)(mi)(Ar)(Gi)(mi),NH=Cf(Ea)(si)(Ar)(Gi)(si),UH=Cf(Fa)(vi)(Ar)(Gi)(vi),BH=Cf(Ta)(_i)(Ar)(Gi)(_i),WH=Cf(uu)(pi)(Ar)(Gi)(pi),qH=Cf(Wi)(li)(Ar)(Gi)(li),HH=Cf(Bi)(ci)(Ar)(Gi)(ci),$f=K_(Ma)(Vi),zH=$f(vn)(Di)(Ar)(Vi)(Di),VH=$f(Ma)(mi)(Ar)(Vi)(mi),GH=$f(Ea)(si)(Ar)(Vi)(si),JH=$f(Fa)(vi)(Ar)(Vi)(vi),jH=$f(Ta)(_i)(Ar)(Vi)(_i),XH=$f(uu)(pi)(Ar)(Vi)(pi),QH=$f(Wi)(li)(Ar)(Vi)(li),KH=$f(Bi)(ci)(Ar)(Vi)(ci),Tf=K_(Ea)(zi),YH=Tf(vn)(Di)(Ar)(zi)(Di),ZH=Tf(Ma)(mi)(Ar)(zi)(mi),tz=Tf(Ea)(si)(Ar)(zi)(si),rz=Tf(Fa)(vi)(Ar)(zi)(vi),ez=Tf(Ta)(_i)(Ar)(zi)(_i),nz=Tf(uu)(pi)(Ar)(zi)(pi),uz=Tf(Wi)(li)(Ar)(zi)(li),az=Tf(Bi)(ci)(Ar)(zi)(ci),Ff=K_(Fa)(Hi),iz=Ff(vn)(Di)(Ar)(Hi)(Di),oz=Ff(Ma)(mi)(Ar)(Hi)(mi),fz=Ff(Ea)(si)(Ar)(Hi)(si),cz=Ff(Fa)(vi)(Ar)(Hi)(vi),lz=Ff(Ta)(_i)(Ar)(Hi)(_i),pz=Ff(uu)(pi)(Ar)(Hi)(pi),_z=Ff(Wi)(li)(Ar)(Hi)(li),vz=Ff(Bi)(ci)(Ar)(Hi)(ci),Ef=K_(Ta)(qi),sz=Ef(vn)(Di)(Ar)(qi)(Di),mz=Ef(Ma)(mi)(Ar)(qi)(mi),Dz=Ef(Ea)(si)(Ar)(qi)(si),bz=Ef(Fa)(vi)(Ar)(qi)(vi),Az=Ef(Ta)(_i)(Ar)(qi)(_i),kz=Ef(uu)(pi)(Ar)(qi)(pi),dz=Ef(Wi)(li)(Ar)(qi)(li),yz=Ef(Bi)(ci)(Ar)(qi)(ci),gz=function(){return 15/40}(),hz=function(){return 10/40}(),Sz=function(){return 7/40}(),xz=function(){return 3/40}(),Cz=function(){return 1/40}(),EF=function(t){return function(r){return function(e){return lH(PH)({analyser:Ue(function(n){return function(u){var a=pH(_H)(u),o=vH(e)(function(f){return f.right}(a)),i=function(f){return f.left}(a);return Lr([on(yr(gr(sH(Ot.value)("cursor: pointer;")))(Mm(t)(function(f){return n(Ct.create(f))})(function(f){return at(f)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(f){return function(m){return function(){var D=Or(P.value)(),A=Sf(f)(),b=mH(Fe(0))(),_=yc([wH(m)(function(G){return function(){return te(new F(G))(D)(),te(P.value)(D)}})])(xc(b)),k=we(yr(ey(Ct.create)(_))(ey(xt.create)(xf)))(function(G){if(G instanceof Ct)return G.value0(A);if(G instanceof xt)return function(){var bt=Ur(D)();return DH(bt)(function(vr){return function(){var z=V_(vr)(),zt=bH(z)(),st=Or(0)(),qr=Or(0)(),Wr=Or(0)(),fn=Or(0)(),On=Or(0)(),ia=Or(0)(),be=Or(0)(),ue=Or(0)(),Wu=Or(0)(),to=Or(0)(),Ec=function(dn){if(dn<32)return st;if(dn<64)return qr;if(dn<96)return Wr;if(dn<128)return fn;if(dn<168)return On;if(dn<160)return ia;if(dn<224)return be;if(Jr)return ue;throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 146, column 45 - line 154, column 63): "+[dn.constructor.name])};bp(zt)(function(dn){var Vf=Rm(dn);return function(){var ro=Ur(to)();return oo(ny(Vf))(Wu)(),oo(ny(Vf))(Ec(ro))(),oo(ny(1))(to)()}})();var lu=AH(function(dn){return function(){var Sv=uy(Br)(Ur(dn))(),ro=uy(cH(Sv))(uy(Br)(Ur(Wu)))();return ay(ro>gz)(iy(ro>hz)(oy(ro>Sz)(fy(ro>xz)(cy(ro>Cz)(Oa)))))}})(kH(st)(dH(qr)(yH(Wr)(ay(fn)(iy(On)(oy(ia)(fy(be)(cy(ue)(Oa)))))))))();return n(new xt(lu))()}})()};throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 127, column 57 - line 165, column 57): "+[G.constructor.name])})();return function(){return k(),function(){var bt=gH(f)();return hH(bt!=="closed")(SH(f))()}(),n(new xt(FF(w(xH(w(!1))))))()}}}})(e)(o)))([Ve(ey(function(f){if(f instanceof ua)return"Turn on";if(f instanceof Ki)return"Loading...";if(f instanceof Yi)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 178, column 31 - line 181, column 56): "+[f.constructor.name])})(o))]),sr(gr($r(Ot.value)("display: grid; grid-template-columns: repeat(8, 1fr); grid-auto-rows: 20px;")))([sr(yr(gr($r(Ot.value)(Tr)))(RH(Nu)(Nu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(LH(Lu)(Nu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(NH(Ru)(Nu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(UH(Iu)(Nu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(BH(Pu)(Nu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(WH(ql)(Nu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(qH(Wl)(Nu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(HH(Bl)(Nu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(zH(Nu)(Lu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(VH(Lu)(Lu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(GH(Ru)(Lu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(JH(Iu)(Lu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(jH(Pu)(Lu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(XH(ql)(Lu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(QH(Wl)(Lu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(KH(Bl)(Lu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(YH(Nu)(Ru)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(ZH(Lu)(Ru)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(tz(Ru)(Ru)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(rz(Iu)(Ru)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(ez(Pu)(Ru)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(nz(ql)(Ru)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(uz(Wl)(Ru)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(az(Bl)(Ru)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(iz(Nu)(Iu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(oz(Lu)(Iu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(fz(Ru)(Iu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(cz(Iu)(Iu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(lz(Pu)(Iu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(pz(ql)(Iu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(_z(Wl)(Iu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(vz(Bl)(Iu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(sz(Nu)(Pu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(mz(Lu)(Pu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(Dz(Ru)(Pu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(bz(Iu)(Pu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(Az(Pu)(Pu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(kz(ql)(Pu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(dz(Wl)(Pu)(i)))([]),sr(yr(gr($r(Ot.value)(Tr)))(yz(Bl)(Pu)(i)))([])])])}})})}}};var Tz=Yt({reflectType:function(){return`<section>
  <h2 id="bandpass">Bandpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">bandpass filter</a> isolates a single frequency range of a source. When you crank up a bandpass node's Q value, the isolation gets more intense. At the extreme, the source signal is almost lost and you get a pure sound that resembles a sine-wave oscillator.</p>

  <pre><code>\\buf -> run2_
  [ fan1 (loopBuf buf bangOn)
    \\b _ -> gain_ 0.8
      [ bandpass_ { frequency: 400.0, q: 1.0 } [ b ]
      , bandpass_ { frequency: 880.0, q: 5.0 } [ b ]
      , bandpass_ { frequency: 1200.0, q: 10.0 } [ b ]
      , bandpass_ { frequency: 2000.0, q: 20.0 } [ b ]
      , bandpass_ { frequency: 3000.0, q: 30.0 } [ b ]
      ]
  ]</code></pre>

  @bandpass@
  </section>
`}})({reflectType:function(){return"@"}})()()(q()(N)({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}})),Fz=tr(cr),Ez=Q(),Mz=Pt(_t),Y_=Xi(ki(Zt(Cr()(dt(dt(xr)(ji)()()()({reflectSymbol:function(){return"q"}}))(bi)()()()({reflectSymbol:function(){return"frequency"}})))(Sr()()))),Oz=function(){return d.value}(),MF=function(t){return function(r){return function(e){return Tz(d.value)(Oz)({bandpass:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([iu(Fz(u)(Ez))(function(a){return Mz(.8)([Y_({frequency:400,q:1})([a]),Y_({frequency:880,q:5})([a]),Y_({frequency:1200,q:10})([a]),Y_({frequency:2e3,q:20})([a]),Y_({frequency:3e3,q:30})([a])])})])}})})}}};var Pz=Dt({reflectType:function(){return`<section>
  <h2 id="compression">Compression</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode">Compression</a>, when used judiciously, can make certain sounds sit better in a mix, like for example vocals. The <a href="https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode">MDN Web Audio documentation</a> does an excellent job explaining how its parameters work. When used not-judiciously, it makes everything sound loud, and who likes that? So let's use it judiciously, like in the example below. We'll pass an object that only specifies the threshold and otherwise use the default options for the compressor.</p>

  <pre><code>
-- defaultDynamicsCompressor =
--   { ratio: 12.0
--   , attack: 0.003
--   , release: 0.25
--   , knee: 30.0
--   , threshold: -24.0
--   }
run2_
  [ dynamicsCompressor_ { threshold: -50.0 }
      [ loopBuf buf bangOn ]
  ]</code></pre>

  ~compression~
  </section>
`}})()()(q()(N)({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}})),Iz=YT(UT(Zt(Cr()(xr))(Sr()()))),Rz=tr(cr),Lz=Q(),Nz=function(){return d.value}(),OF=function(t){return function(r){return function(e){return Pz(Nz)({compression:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([Iz({})([Rz(u)(Lz)])])}})})}}};var Mf=po(),Of=Eo();var Bz=Of({reflectSymbol:function(){return"playbackRate"}}),Wz=Of({reflectSymbol:function(){return"onOff"}}),qz=Of({reflectSymbol:function(){return"offset"}}),Hz=Of({reflectSymbol:function(){return"loopStart"}}),zz=Of({reflectSymbol:function(){return"loopEnd"}}),Vz=Of({reflectSymbol:function(){return"gain"}}),Gz=Of({reflectSymbol:function(){return"frequency"}});var Jz=Of({reflectSymbol:function(){return"delayTime"}});var Uu=function(){return function(t){var r=Bz(d.value),e=dc(t);return function(n){return Mf(r(e(n)))}}},Cc=function(){return function(t){var r=Wz(d.value),e=L0(t);return function(n){return Mf(r(e(n)))}}},wF=function(){return function(t){var r=qz(d.value),e=dc(t);return function(n){return Mf(r(e(n)))}}},PF=function(){var t=Hz(d.value);return function(r){return Mf(t(r))}},IF=function(){var t=zz(d.value);return function(r){return Mf(t(r))}},Du=function(){return function(t){var r=Vz(d.value),e=dc(t);return function(n){return Mf(r(e(n)))}}},wf=function(){return function(t){var r=Gz(d.value),e=dc(t);return function(n){return Mf(r(e(n)))}}};var ly=function(){return function(t){var r=Jz(d.value),e=dc(t);return function(n){return Mf(r(e(n)))}}};var py=q(),jz=Dt({reflectType:function(){return`<section>
  <h2 id="constant">Constant value</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode">Constant values</a>, or DC offset, is a way to output an unchanging stream of values. This is only really useful when testing the performance of speakers or microphones and/or when working with a custom audio node that supports constant streaming values. Note that the constant source node in the web audio API can <i>also</i> be used to control audio parameters. Ocarina uses this feature of constant nodes under the hood to optimize certain computations.</p>

  <p>The following example abuses a constant audio node by turning it into a gnarly inpulse generator. We'll learn about the tie fighter symbol <code>~tf~</code> and the <code>pure</code> in the next section on Events. Kids, don't try this at home!</p>

  <pre><code>~txt~</code></pre>

  ~constant~
  </section>
`}})()()(py(py(py(N)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"tf"}})({reflectSymbol:function(){return"tf"}}))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}})),Xz=M(ru),Qz=Pt(_t),Kz=Sm(vm),Yz=ht(qt),Zz=Q(),tV=M(X),rV=wF()(En),eV=Tu(Da),nV=hu(Ga),uV=function(){return d.value}(),RF=function(t){return function(r){return function(e){return jz(uV)({tf:C("<|>"),txt:C(`run2_
  [ gain_ 0.5
      [ constant 0.0
          ( bangOn <|>
              ( pure $ offset $ AudioEnvelope
                  { d: 5.0
                  , o: 0.1
                  , p: 0 .. 1920 # mapWithIndex
                      \\i -> const $
                      if i \`mod\` 3 == 0 then 1.0
                      else 0.0
                  }
              )
          )
      ]
  ]`),constant:vt(e)(t)(function(n){return Xz(void 0)})(function(n){return function(u){return Y(n)([Qz(.5)([Kz(0)(Yz(Zz)(tV(rV({d:5,o:.1,p:eV(function(a){return w(function(){var o=nV(a)(3)===0;return o?1:0}())})(Ke(0)(1920))}))))])])}})})}}};var iV=Dt({reflectType:function(){return`<section>
  <h2 id="convolution">Convolution</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConvolverNode">Convolution</a>, aka reverb, is a way to graft the shape of one sound (usually an <a href="https://en.wikipedia.org/wiki/Impulse_response">impulse response</a>) onto another. Convolution can sound great, but it is a <i>very expensive operation</i> that will cause noticeable artifacts on low-end devices. When shipping audio code to production, you're usually better off using an Audio Worklet Node with reverb optimized for your specific case. That said, for PoCs or hobbyist projects, convolution is great!</p>

  <pre><code>\\{loop, verb} -> run2_
  [ convolver verb [ loopBuf loop bangOn ] ]</code></pre>

  ~convolution~
  </section>
`}})()()(q()(N)({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}})),oV=it(sC),fV=S(To),cV=GT(tT),lV=tr(cr),pV=Q(),_V=function(){return d.value}(),LF=function(t){return function(r){return function(e){return iV(_V)({convolution:vt(e)(t)(function(n){return oV(fV(function(u){return function(a){return{loop:u,verb:a}}})(at(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")))(at(n)("https://cdn.jsdelivr.net/gh/andibrae/Reverb.js/Library/StMarysAbbeyReconstructionPhase3.m4a"))})(function(n){return function(u){return Y(n)([cV(u.verb)([lV(u.loop)(pV)])])}})})}}};var sV=Yt({reflectType:function(){return`<section>
  <h2 id="delay">Delay</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/DelayNode">Delay</a>, as its name suggests, delays a signal. Using multiple delay nodes, you can create a decent echo effect.</p>

  <p>To create an even <i>better</i> echo effect, you can used fixed points, which is covered in the <a>Fix and fan</a> section of this documentation.</p>

  <pre><code>\\buf -> run2_
  [ fan1 (playBuf buf bangOn)
      \\b _ -> gain_ 0.2
        [ delay_ 0.03 [ b ]
        , delay_ 0.1 [ b ]
        , delay_ 0.3 [ b ]
        , delay_ 0.7 [ b ]
        ]
  ]</code></pre>

  @delay@
  </section>
`}})({reflectType:function(){return"@"}})()()(q()(N)({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}})),mV=au(Ia),DV=Q(),bV=Pt(_t),Nm=hf(Ai),AV=function(){return d.value}(),NF=function(t){return function(r){return function(e){return sV(d.value)(AV)({delay:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(u){return Y(n)([iu(mV(u)(DV))(function(a){return bV(.2)([Nm(.03)([a]),Nm(.1)([a]),Nm(.3)([a]),Nm(.7)([a])])})])}})})}}};var dV=Dt({reflectType:function(){return`<section>
  <h2 id="gain">Gain</h2>
  <p>The almighty <a href="https://developer.mozilla.org/en-US/docs/Web/API/GainNode">gain</a> node is your friendly neighborhood volume control. Volume in the web-audio API goes from 0 to 1 whereas we hear logarithmically, so when you're using this, make sure to convert between decibels and gain if you want to work with more intuitive units. The conversion formula is as follows:</p>

  <pre><code>decibels = 20 * log10( gain );</code></pre>

  <p>And here's a little example of a single gain node:</p>

  <pre><code>run2_
  [ gain_ 0.1
    [ loopBuf buf bangOn ]
  ] </code></pre>

  ~gain~
  </section>
`}})()()(q()(N)({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}})),yV=Pt(_t),gV=tr(cr),hV=Q(),SV=function(){return d.value}(),UF=function(t){return function(r){return function(e){return dV(SV)({gain:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(u){return Y(n)([yV(.1)([gV(u)(hV)])])}})})}}};var CV=Dt({reflectType:function(){return`<section>
  <h2 id="highpass">Highpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highpass filter</a> lets higher frequencies pass and amortizes lower ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ highpass_ 2000.0
      [ loopBuf buf bangOn ]
  ]
</code></pre>

  ~highpass~
  </section>
`}})()()(q()(N)({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}})),$V=Rl(Pl),TV=tr(cr),FV=Q(),EV=function(){return d.value}(),BF=function(t){return function(r){return function(e){return CV(EV)({highpass:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([$V(2e3)([TV(u)(FV)])])}})})}}};var OV=Dt({reflectType:function(){return`<section>
  <h2 id="highshelf">Highshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highshelf filter</a> boosts or attenuates high frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
  [ highshelf_ { frequency: 2000.0, gain: 0. }
      [ loopBuf buf bangOn ]
  ]</code></pre>

  ~highshelf~
  </section>
`}})()()(q()(N)({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}})),wV=ZT(NT(Zt(Cr()(dt(dt(xr)(_T)()()()({reflectSymbol:function(){return"gain"}}))(vT)()()()({reflectSymbol:function(){return"frequency"}})))(Sr()()))),PV=tr(cr),IV=Q(),RV=function(){return d.value}(),WF=function(t){return function(r){return function(e){return OV(RV)({highshelf:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([wV({frequency:2e3,gain:.4})([PV(u)(IV)])])}})})}}};var NV=Dt({reflectType:function(){return`<section>
  <h2 id="iir">IIR filter</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/IIRFilterNode">IIR filter</a>, or infinite impulse response filter, is the Swiss Army Knife of filters. You can carve out and boost parts of the spectrum with amazing precision. But it comes with a catch: you can't automate the parameters. The parameters are also tough to work with if you're new to IIR filters. In short, you're setting up coefficients for a filter of type:</p>

  <pre><code>x0s0 + x1s1 + x2s2 + ... + y0S0 + y1S1 + y2S2 + ...</code></pre>

  <p>Where <code>s1</code> is the unfiltered signal at time <code>t-1</code>, <code>S0</code> is the <i>filtered</i> signal at time <code>t-1</code>, etc. The xs and ys are often called <i>feedforward</i> and <i>feedback</i> coefficients respectively.</p>

  <p>Because the Web Audio API accepts between 3 and 20 parameters for feedforward and feedback coefficients, Ocarina enforces that through a <a href="https://github.com/bodil/purescript-sized-vectors">sized vector</a>.</p>

  <pre><code>\\{loop, verb} -> run2_
  [ iirFilter
      ( (0.00020298 : 0.0004059599 : 0.00020298 : empty)
          /\\ (1.0126964558 : -1.9991880801 : 0.9873035442 : empty)
      )
      [ loopBuf buf bangOn ]
  ]</code></pre>
  ~iirFilterEx~
  </section>
`}})()()(q()(N)({reflectType:function(){return"iirFilterEx"}})({reflectSymbol:function(){return"iirFilterEx"}})),UV=JT()()(Z0(lc)(lc)),Hl=nx()(),BV=tr(cr),WV=Q(),qV=function(){return d.value}(),qF=function(t){return function(r){return function(e){return NV(qV)({iirFilterEx:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([UV(new U(Hl(20298e-8)(Hl(.0004059599)(Hl(20298e-8)(Vb))),Hl(1.0126964558)(Hl(-1.9991880801)(Hl(.9873035442)(Vb)))))([BV(u)(WV)])])}})})}}};var zV=Yt({reflectType:function(){return`<section>
  <h2 id="loopbuf">Looping buffer</h2>

  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode">looping buffer</a> is buffered audio that loops. The buffered audio is usually a sound file, but it'll play anything you write to a buffer. Like in the Web Audio API, you can set the buffer's start and end and optionally its duration.</p>

  <pre><code>\\buf -> run2_
  [ loopBuf
      { buffer: buf
      , playbackRate: 0.5
      , loopStart: 0.1
      , loopEnd: 0.6
      }
      bangOn
  , loopBuf
      { buffer: buf
      , playbackRate: 1.0
      , loopStart: 0.5
      , loopEnd: 1.2
      }
      bangOn
  , loopBuf
      { buffer: buf
      , playbackRate: 1.7
      }
      bangOn
  ]</code></pre>

  @loopBuf@
  </section>
`}})({reflectType:function(){return"@"}})()()(q()(N)({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}})),zF=Cr(),VF=dt(dt(xr)(Dm)()()()({reflectSymbol:function(){return"playbackRate"}})),GF={reflectSymbol:function(){return"buffer"}},JF=Sr()(),HF=tr(wl(Zt(zF(dt(dt(VF(mm)()()()({reflectSymbol:function(){return"loopStart"}}))(sm)()()()({reflectSymbol:function(){return"loopEnd"}}))(El)()()()(GF)))(JF))),_y=Q(),VV=tr(wl(Zt(zF(VF(El)()()()(GF)))(JF))),GV=function(){return d.value}(),jF=function(t){return function(r){return function(e){return zV(d.value)(GV)({loopBuf:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/100/100981_1234256-lq.mp3")})(function(n){return function(u){return Y(n)([HF({buffer:u,playbackRate:.5,loopStart:.1,loopEnd:.6})(_y),HF({buffer:u,playbackRate:1,loopStart:.5,loopEnd:1.2})(_y),VV({buffer:u,playbackRate:1.7})(_y)])}})})}}};var jV=Dt({reflectType:function(){return`<section>
  <h2 id="lowpass">Lowpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowpass filter</a> lets lower frequencies pass and amortizes higher ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ lowpass_ 215.0 [ loopBuf buf bangOn ] ]
</code></pre>

  ~lowpass~
  </section>
`}})()()(q()(N)({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}})),XV=Ll(bm),QV=tr(cr),KV=Q(),YV=function(){return d.value}(),XF=function(t){return function(r){return function(e){return jV(YV)({lowpass:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([XV(215)([QV(u)(KV)])])}})})}}};var t5=Dt({reflectType:function(){return`<section>
  <h2 id="lowshelf">Lowshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowshelf filter</a> boosts or attenuates lower frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
   [ lowshelf_ { frequency: 91.0, gain: 10.0 }
       [ loopBuf buf bangOn ]
   ]
</code></pre>

  ~lowshelf~
  </section>
`}})()()(q()(N)({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}})),r5=rF(LT(Zt(Cr()(dt(dt(xr)(cT)()()()({reflectSymbol:function(){return"gain"}}))(lT)()()()({reflectSymbol:function(){return"frequency"}})))(Sr()()))),e5=tr(cr),n5=Q(),u5=function(){return d.value}(),QF=function(t){return function(r){return function(e){return t5(u5)({lowshelf:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([r5({frequency:91,gain:.4})([e5(u)(n5)])])}})})}}};var i5=Yt({reflectType:function(){return`<section>
  <h2 id="microphone">Microphone</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioSourceNode">microphone</a> will use your microphone if you give the browser permission to do so.</p>

  <blockquote>Make sure to use \u{1F3A7} when you run this example! Otherwise, you'll cause quite a stir in whatever internet cafe, household or public restroom you're perusing this documentation in.</blockquote>

  <pre><code>\\mic -> run2_
  [ case mic of
      Just m -> fix \\i -> gain_ 1.0
        [ microphone m
        , delay_ 0.1 [ gain_ 0.2 [ input i ] ]
        ]
      Nothing -> gain_ 0.02 [ sinOsc_ 440.0 ]
  ]</code></pre>

  @microphone@
  </section>
`}})({reflectType:function(){return"@"}})()()(q()(N)({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}})),vy=Pt(_t),o5=R_(M_),f5=hf(Ai),c5=QT(gf),l5=function(){return d.value}(),KF=function(t){return function(r){return function(e){return i5(d.value)(l5)({microphone:vt(e)(t)(function(n){return Fm(!0)(!1)})(function(n){return function(u){return Y(n)([function(){if(u.microphone instanceof F)return wu(function(a){return vy(1)([o5(u.microphone.value0),f5(.1)([vy(.2)([a])])])});if(u.microphone instanceof P)return vy(.02)([c5(440)]);throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Microphone (line 44, column 15 - line 49, column 56): "+[u.microphone.constructor.name])}()])}})})}}};var _5=Dt({reflectType:function(){return`<section>
<h2 id="notch">Notch filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">notch filter</a>, also known as a band-reject filter, attenuates a single frequency range of a source. When you crank up their Q value, the attenuation gets more intense. At the extreme, it sounds like part of the source got sucked into a vacuum, which is not un-interesting!</p>

  <pre><code>\\buf -> run2_
  [
    notch_ { frequency: 400.0, q: 1.0 }
    $ pure $ notch_ { frequency: 880.0, q: 5.0 }
    $ pure $ notch_ { frequency: 1200.0, q: 10.0 }
    $ pure $ notch_ { frequency: 2000.0, q: 20.0 }
    $ pure $ notch_ { frequency: 3000.0, q: 30.0 }
    $ pure $ loopBuf buf bangOn
  ]</code></pre>

  ~notch~
  </section>
`}})()()(q()(N)({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}})),Z_=eF(RT(Zt(Cr()(dt(dt(xr)(oT)()()()({reflectSymbol:function(){return"q"}}))(fT)()()()({reflectSymbol:function(){return"frequency"}})))(Sr()()))),tv=M(Ha),v5=tr(cr),s5=Q(),m5=function(){return d.value}(),YF=function(t){return function(r){return function(e){return _5(m5)({notch:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([Z_({frequency:400,q:1})(tv(Z_({frequency:880,q:5})(tv(Z_({frequency:1200,q:10})(tv(Z_({frequency:2e3,q:20})(tv(Z_({frequency:3e3,q:30})(tv(v5(u)(s5)))))))))))])}})})}}};var b5=Dt({reflectType:function(){return`<section>
  <h2 id="peaking">Peaking filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">peaking filter</a> is sort of like a notch/bandpass combo. It sounds different than bandpass or notch, and is often a better choice depending on what you're making. The Q works as normal, but the gain either boosts or attenuates the frequency in question if it is positive or negative.</p>

  <pre><code>\\buf -> run2_
  [
    peaking_ { frequency: 400.0, q: 1.0, gain: -20.0 }
    $ pure $ peaking_ { frequency: 880.0, q: 5.0, gain: 20.0 }
    $ pure $ peaking_ { frequency: 1200.0, q: 10.0, gain: -20.0 }
    $ pure $ peaking_ { frequency: 2000.0, q: 20.0, gain: 20.0 }
    $ pure $ peaking_ { frequency: 3000.0, q: 30.0, gain: -20.0 }
    $ pure $ loopBuf buf bangOn
  ]</code></pre>

  ~peaking~
  </section>
`}})()()(q()(N)({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}})),rv=uF(IT(Zt(Cr()(dt(dt(dt(xr)(uT)()()()({reflectSymbol:function(){return"q"}}))(aT)()()()({reflectSymbol:function(){return"gain"}}))(iT)()()()({reflectSymbol:function(){return"frequency"}})))(Sr()()))),ev=M(Ha),A5=tr(cr),k5=Q(),d5=function(){return d.value}(),ZF=function(t){return function(r){return function(e){return b5(d5)({peaking:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([rv({frequency:400,q:1,gain:-20})(ev(rv({frequency:880,q:5,gain:20})(ev(rv({frequency:1200,q:10,gain:-20})(ev(rv({frequency:2e3,q:20,gain:20})(ev(rv({frequency:3e3,q:30,gain:-20})(ev(A5(u)(k5)))))))))))])}})})}}};var g5=Dt({reflectType:function(){return`<section>
  <h2 id="periodic">Periodic wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">periodic wave oscillator</a> plays back a custom periodic waveform at a given frequency. The custom waveform must be set as part of the initialization and can be changed after initialization. Note that the change will not go into effect if the oscillator is on: it must be turned off and on again.</p>

  <pre><code>\\buf -> run2_
  [
    gain_ 0.2
      [
        periodicOsc
          { frequency: 140.0
          , spec:
              ( (0.1 +> 0.2 +> 0.3 +> 0.4 +> empty)
                  /\\ (0.4 +> 0.3 +> 0.2 +> 0.1 +> empty)
              )
          }
          bangOn
      ]
  ]</code></pre>

  ~periodic~
  </section>
`}})()()(q()(N)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})),h5=M(ru),S5=Pt(_t),x5=Il(Ol(Zt(Cr()(dt(dt(xr)(Ml($l(fi)))()()()({reflectSymbol:function(){return"spec"}}))(Fl)()()()({reflectSymbol:function(){return"frequency"}})))(Sr()()))),tE=Pe(Se(fi)()(ta)()(Po)),rE=Pe(Se(Mo)()(ra)()(ta)),eE=Pe(Se(Oo)()(ea)()(ra)),nE=Pe(Se(wo)()(Io)()(ea)),C5=Q(),$5=function(){return d.value}(),uE=function(t){return function(r){return function(e){return g5($5)({periodic:vt(e)(t)(function(n){return h5(void 0)})(function(n){return function(u){return Y(n)([S5(.2)([x5({frequency:140,spec:new U(tE(.1)(rE(.2)(eE(.3)(nE(.4)(Oa)))),tE(.4)(rE(.3)(eE(.2)(nE(.1)(Oa)))))})(C5)])])}})})}}};var F5=Dt({reflectType:function(){return`<section>
  <h2 id="playbuf">Playing a buffer</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode">Playback from a buffer</a> is one of the bread-and-butter operations in Web Audio (or any audio). The buffered audio is usually a sound file, but it'll play anything you write to a buffer. Like in the Web Audio API, you can set the buffer's start time and optionally its duration.</p>

  <pre><code>\\buf -> run2_
  [
    playBuf
      { buffer
      , duration: 3.0
      , bufferOffset: 4.2
      }
      bangOn
  ]
</code></pre>

  ~playBuf~
  </section>
`}})()()(q()(N)({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}})),E5=au(w_(Zt(Cr()(dt(dt(dt(xr)(eT)()()()({reflectSymbol:function(){return"duration"}}))(rT)()()()({reflectSymbol:function(){return"bufferOffset"}}))(O_)()()()({reflectSymbol:function(){return"buffer"}})))(Sr()()))),M5=Q(),O5=function(){return d.value}(),aE=function(t){return function(r){return function(e){return F5(O5)({playBuf:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/470/470035_9564355-lq.mp3")})(function(n){return function(u){return Y(n)([E5({buffer:u,duration:3,bufferOffset:4.2})(M5)])}})})}}};var sy=function(){function t(){}return t.value=new t,t}();var iE={attr:function(t){return function(r){return c({key:"controls",value:s(r)})}}};var my=function(){function t(){}return t.value=new t,t}();var oE={attr:function(t){return function(r){return c({key:"src",value:s(r)})}}};var Dy=O("audio");var $c=function(){function t(){this.head=null,this.last=null,this.size=0}function r(v,D){this.queue=v,this.value=D,this.next=null,this.prev=null}function e(v){this.draining=!1,this.error=null,this.value=v,this.takes=new t,this.reads=new t,this.puts=new t}var n={};function u(v){try{v()}catch(D){setTimeout(function(){throw D},0)}}function a(v,D){var A=new r(v,D);switch(v.size){case 0:v.head=A;break;case 1:A.prev=v.head,v.head.next=A,v.last=A;break;default:A.prev=v.last,v.last.next=A,v.last=A}return v.size++,A}function o(v){var D;switch(v.size){case 0:return null;case 1:D=v.head,v.head=null;break;case 2:D=v.last,v.head.next=null,v.last=null;break;default:D=v.last,v.last=D.prev,v.last.next=null}return D.prev=null,D.queue=null,v.size--,D.value}function i(v){var D;switch(v.size){case 0:return null;case 1:D=v.head,v.head=null;break;case 2:D=v.head,v.last.prev=null,v.head=v.last,v.last=null;break;default:D=v.head,v.head=D.next,v.head.prev=null}return D.next=null,D.queue=null,v.size--,D.value}function f(v){if(v.queue!==null){if(v.queue.last===v){o(v.queue);return}if(v.queue.head===v){i(v.queue);return}v.prev&&(v.prev.next=v.next),v.next&&(v.next.prev=v.prev),v.queue.size--,v.queue=null,v.value=null,v.next=null,v.prev=null}}function m(v,D){if(!D.draining){var A=D.puts,b=D.takes,_=D.reads,k,G,nt,bt,vr;for(D.draining=!0;;){if(k=null,G=null,nt=null,bt=D.value,vr=_.size,D.error!==null){for(bt=v.left(D.error);k=i(A);)u(k.cb(bt));for(;G=i(_);)u(G(bt));for(;nt=i(b);)u(nt(bt));break}if(bt===n&&(k=i(A))&&(D.value=bt=k.value),bt!==n){for(nt=i(b);vr--&&(G=i(_));)u(G(v.right(bt)));nt!==null&&(D.value=n,u(nt(v.right(bt))))}if(k!==null&&u(k.cb(v.right(void 0))),D.value===n&&A.size===0||D.value!==n&&b.size===0)break}D.draining=!1}}return e.EMPTY=n,e.putLast=a,e.takeLast=o,e.takeHead=i,e.deleteCell=f,e.drainVar=m,e}();function by(){return new $c($c.EMPTY)}function fE(t,r,e){return function(){return e.value===$c.EMPTY&&e.error===null?(e.value=r,$c.drainVar(t,e),!0):!1}}function cE(t,r){return function(){var e=r.value;return e===$c.EMPTY?t.nothing:(r.value=$c.EMPTY,$c.drainVar(t,r),t.just(e))}}var N5=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),U5=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),B5=function(){function t(){}return t.value=new t,t}();var lE=function(){return{left:xt.create,right:Ct.create,nothing:P.value,just:F.create,killed:N5.create,filled:U5.create,empty:B5.value}}();var pE=function(t){return function(r){return fE(lE,t,r)}};var _E=function(t){return cE(lE,t)};var q5=Dt({reflectType:function(){return`<section>
  <h2 id="recorder">Recorder</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioDestinationNode">recorder</a> allows you to record audio. It takes a callback that you can use to stash the recorded audio somewhere, like in a file for example, as the example below does. You can use it as a simple note-taking app \u{1F399}\uFE0F.</p>

  <pre><code>\\cb m -> recorder cb (microphone m)</code></pre>

  ~recorder~
  </section>
`}})()()(q()(N)({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}})),vE=Qv(ya),sE=K(Z),H5=Q_(qt)(X),zl=ht(qt),nv=M(X),z5=mt(ol),Pf=S(ct),V5=mt(ke),uv=M(Wt),Ay=rr(or),G5=$u(Wt)(Me),mE=ks(tC),J5=lt(Qn),j5=lt(qn),X5=S(To),Q5=de(Ge),K5=Mn(ne),Y5=Aa(ba),Z5=Pr(L),tG=Dn(Wt)(Me),rG=J_(ne),eG=Pn(Wt),nG=mn(ne),DE=it(pn),uG=mt(iE),bE=mt(Wx),aG=mt(oE),iG=function(t){var r=Ik(t);return function(e){var n=R_(e);return function(u){return function(a){return r(a)(n(u))}}}},oG=iG(Ak)(M_),fG=function(){return d.value}(),AE=function(t){return function(r){return function(e){return q5(fG)({recorder:Ue(function(n){return function(u){var a=vE(sE)(u),o=vE(sE)(function(v){return v.left}(a)),i=function(v){return v.right}(o),f=H5(e)(function(v){return v.right}(a)),m=function(v){return v.left}(o);return Lr([on(zl(nv(z5(Ot.value)("cursor: pointer;")))(Pf(function(v){return V5(ee.value)(Vr(w(function(){if(v.e instanceof Ki)return uv(void 0);if(v.e instanceof Yi)return Ay(Ay(Ay(v.e.value0)(t(uv(void 0))))(G5(v.rec)(function(D){return mE($m(D))})))(n(Ct.create(ua.value)));if(v.e instanceof ua)return function(){v.cncl();var A=by();n(new Ct(Ki.value))();var b=Ca(j5(X5(function(_){return _.microphone})(Fm(!0)(!1)))(function(_){return Q5(function(){var G=gt(uv(uv(void 0)))(function(nt){return function(){var vr=K5(),Gr=Sf(vr)(),z=Y5(Fe(0))(),zt=yc([oG(nt)(function(qr){return function(){return n(new xt(new Ct(qr)))(),Z5(pE(qr)(A))(),sF("audio/ogg; codecs=opus")(function(fn){return n(xt.create(xt.create(fn)))})(qr)()}})])(xc(z)),st=we(zt)(function(qr){return qr(Gr)})();return function(){st(),J5(_E(A))(tG(function(fn){return mE($m(fn))}))();var Wr=rG(vr)();return eG(Wr!=="closed")(nG(vr))()}}})(_)();return n(new Ct(new Yi(G)))(),G})}))();return t(function(){return n(Ct.create(ua.value))(),ii(Ro(b))()})(),void 0};throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Recorder (line 67, column 45 - line 111, column 50): "+[v.e.constructor.name])}())))})(DE(Pf(Mc)(DE(Pf(function(v){return function(D){return function(A){return{e:v,cncl:D,rec:A}}}})(f))(zl(nv(uv(void 0)))(Pf(function(v){return v.value0})(e)))))(zl(nv(P.value))(Pf(F.create)(i))))))([Ve(Pf(function(v){if(v instanceof ua)return"Turn on";if(v instanceof Ki)return"Loading...";if(v instanceof Yi)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Recorder (line 123, column 29 - line 126, column 54): "+[v.constructor.name])})(f))]),Lr([Dy(zl(nv(uG(sy.value)("true")))(zl(nv(bE(Ot.value)("display:none;")))(zl(Pf(function(v){return aG(my.value)(v)})(m))(Pf(w(bE(Ot.value)("display:block;")))(m)))))([])])])}})})}}};var lG=Dt({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sawtoothOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(q()(N)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})),pG=M(ru),_G=Pt(_t),vG=XT(Y0),sG=Q(),mG=function(){return d.value}(),kE=function(t){return function(r){return function(e){return lG(mG)({periodic:vt(e)(t)(function(n){return pG(void 0)})(function(n){return function(u){return Y(n)([_G(.2)([vG(448)(sG)])])}})})}}};var bG=Dt({reflectType:function(){return`<section>
  <h2 id="sine">Sine wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sine wave oscillator</a> plays back a sine wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sinOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(q()(N)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})),AG=M(ru),kG=Pt(_t),dG=gc(gf),yG=Q(),gG=function(){return d.value}(),dE=function(t){return function(r){return function(e){return bG(gG)({periodic:vt(e)(t)(function(n){return AG(void 0)})(function(n){return function(u){return Y(n)([kG(.2)([dG(448)(yG)])])}})})}}};var SG=Dt({reflectType:function(){return`<section>
  <h2 id="sawtooth">Square wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ squareOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(q()(N)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})),xG=M(ru),CG=Pt(_t),$G=L_(Tl),TG=Q(),FG=function(){return d.value}(),yE=function(t){return function(r){return function(e){return SG(FG)({periodic:vt(e)(t)(function(n){return xG(void 0)})(function(n){return function(u){return Y(n)([CG(.2)([$G(448)(TG)])])}})})}}};var MG=Dt({reflectType:function(){return`<section>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/StereoPannerNode">stereo panner</a> pans audio in the stereo plane. <code>-1.0</code> represents hard left, and <code>1.0</code> represents hard right, as in the example below.</p>

  <pre><code>\\buf -> run2_
  [ pan_ 1.0 [ loopBuf buf bangOn ] ]</code></pre>

  ~pan~
  </section>
`}})()()(q()(N)({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}})),OG=nF(K0),wG=tr(cr),PG=Q(),IG=function(){return d.value}(),gE=function(t){return function(r){return function(e){return MG(IG)({pan:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(u){return Y(n)([OG(1)([wG(u)(PG)])])}})})}}};var LG=function(){return d.value}(),hE=Dt({reflectType:function(){return`<ul>
  <li><a href="#allpass">All-pass filter</a></li>
  <li><a href="#analyser">Analyser</a></li>
  <li><a href="#bandpass">Bandpass filter</a></li>
  <li><a href="#constant">Constant value</a></li>
  <li><a href="#compression">Compression</a></li>
  <li><a href="#convolution">Convolution</a></li>
  <li><a href="#delay">Delay</a></li>
  <li><a href="#gain">Gain</a></li>
  <li><a href="#highpass">Highpass filter</a></li>
  <li><a href="#highshelf">Highshelf filter</a></li>
  <li><a href="#iir">IIR filter</a></li>
  <li><a href="#loopbuf">Looping buffer</a></li>
  <li><a href="#lowpass">Lowpass filter</a></li>
  <li><a href="#lowshelf">Lowshelf filter</a></li>
  <!--<li><a href="#media">Media element</a></li>-->
  <li><a href="#microphone">Microphone</a></li>
  <li><a href="#notch">Notch filter</a></li>
  <!--<li><a href="#panner">Panner</a></li>-->
  <li><a href="#peaking">Peaking filter</a></li>
  <li><a href="#periodic">Periodic wave oscillator</a></li>
  <li><a href="#playbuf">Playing a buffer</a></li>
  <li><a href="#recorder">Recorder</a></li>
  <li><a href="#sawtooth">Sawtooth wave oscillator</a></li>
  <li><a href="#sine">Sine wave oscillator</a></li>
  <li><a href="#square">Square wave oscillator</a></li>
  <li><a href="#pan">Stereo panner</a></li>
  <li><a href="#triangle">Triangle wave oscillator</a></li>
  <li><a href="#waveshaper">Waveshaper</a></li>
    </ul>
`}})()()(N)(LG)({});var UG=Dt({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ triangleOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(q()(N)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})),BG=M(ru),WG=Pt(_t),qG=hm(_m),HG=Q(),zG=function(){return d.value}(),SE=function(t){return function(r){return function(e){return UG(zG)({periodic:vt(e)(t)(function(n){return BG(void 0)})(function(n){return function(u){return Y(n)([WG(.2)([qG(448)(HG)])])}})})}}};var xE=q(),GG=Dt({reflectType:function(){return`<section>
  <h2 id="waveshaper">Waveshaper</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/WaveshaperNode">waveshaper node</a>, aka distortion, uses a <a href="https://en.wikipedia.org/wiki/Waveshaper">waveshaping function</a> to add warmth to a sound.</p>

  <pre><code>~code~</code></pre>

  ~waveShaper~
  </section>
`}})()()(xE(xE(N)({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}}))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}})),JG=S(ir),jG=wD(za)(jf),XG=aF(PT),QG=tr(cr),KG=Q(),YG=function(){return d.value}(),CE=function(t){return function(r){return function(e){return GG(YG)({code:C(`do
  let
    makeDistortionCurve :: Number -> Array Number
    makeDistortionCurve k =
      map
        ( \\i ->
            let
              x = (toNumber i * 2.0 / toNumber n_samples) - 1.0
            in
              (3.0 + k) * x * 20.0 * deg / (pi + (k * abs x))
        )
        (0 .. (n_samples - 1))
      where
      n_samples = 44100

      deg = pi / 180.0
  run2_
    [ waveShaper (makeFloatArray (makeDistortionCurve 400.0)) [ loopBuf buf bangOn ] ]`),waveShaper:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(u){var a=function(o){var i=sf/180;return JG(function(f){var m=Br(f)*2/Br(44100)-1;return(3+o)*m*20*i/(sf+o*jG(m))})(Ke(0)(44099))};return Y(n)([XG(Hd(a(400)))([QG(u)(KG)])])}})})}}};var t8=rr(or),ve=q(),r8=Dt({reflectType:function(){return`<div>
  <h1>Audio Units</h1>

  <h3>There sure are a lot of them!</h3>
  <p>
    This section provides a tour of the web audio nodes provided by the Web Audio API and, by extension, Ocarina. There are only two omissions:</p>
    <ul>
      <li>Audio Worklet Nodes</li>
      <li>Multi-channel audio</li>
    </ul>
    <p>Both of these will be covered in later sections.</p>

  <p>
    This section is long and should be read like those passages in the Bible that list who was the child of who: DILIGENTLY AND COPIOUSLY. That said, if you want to skip around, here's a table of contents.
  </p>
  ~toc~
  <p>And now, without further ado... (~drumroll~) Here are some audio nodes!</p>

  ~allpass~
  ~analyser~
  ~bandpass~
  ~constant~
  ~compression~
  ~convolution~
  ~delay~
  ~gain~
  ~highpass~
  ~highshelf~
  ~iirFilter~
  ~loopBuf~
  ~lowpass~
  ~lowshelf~
  ~microphone~
  ~notch~
  ~peaking~
  ~periodicOsc~
  ~playBuf~
  ~recorder~
  ~sawtoothOsc~
  ~sinOsc~
  ~squareOsc~
  ~pan~
  ~triangleOsc~
  ~waveShaper~

  <h2>Next steps</h2>
  <p>Phew, that was a lot of audio units! In the next section, we'll make them come alive thanks to the magic of <a ~next~ style="cursor:pointer;">events</a>.</p>
</div>`}})()()(ve(ve(ve(ve(ve(ve(ve(ve(ve(ve(ve(ve(ve(ve(ve(ve(Je()(ve(ve(ve(ve(ve(ve(ve(ve(ve(ve(ve(ve(N)({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}}))({reflectType:function(){return"triangleOsc"}})({reflectSymbol:function(){return"triangleOsc"}}))({reflectType:function(){return"toc"}})({reflectSymbol:function(){return"toc"}}))({reflectType:function(){return"squareOsc"}})({reflectSymbol:function(){return"squareOsc"}}))({reflectType:function(){return"sinOsc"}})({reflectSymbol:function(){return"sinOsc"}}))({reflectType:function(){return"sawtoothOsc"}})({reflectSymbol:function(){return"sawtoothOsc"}}))({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}}))({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}}))({reflectType:function(){return"periodicOsc"}})({reflectSymbol:function(){return"periodicOsc"}}))({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}}))({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}}))({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}}))({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}}))({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}}))({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}}))({reflectType:function(){return"iirFilter"}})({reflectSymbol:function(){return"iirFilter"}}))({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}}))({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}}))({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}}))({reflectType:function(){return"drumroll"}})({reflectSymbol:function(){return"drumroll"}}))({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}}))({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}}))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}}))({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}}))({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}}))({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}}))({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}})),e8=Pt(_t),n8=tr(cr),u8=Q(),a8=ou(qt)(X),i8=function(){return d.value}(),$E=function(t){return function(r){return function(e){return function(n){var u=t8(r(Sc.value))(Xe),a=fu(t)(e);return r8(i8)({drumroll:Om("\u{1F941}")(n)(a)(function(o){return at(o)("https://freesound.org/data/previews/50/50711_179538-lq.mp3")})(function(o){return function(i){return Y(o)([e8(1)([n8(i)(u8)])])}}),toc:hE,allpass:hF(a)(r)(n),analyser:EF(a)(r)(n),bandpass:MF(a)(r)(n),constant:RF(a)(r)(n),compression:OF(a)(r)(n),convolution:LF(a)(r)(n),delay:NF(a)(r)(n),gain:UF(a)(r)(n),highpass:BF(a)(r)(n),highshelf:WF(a)(r)(n),iirFilter:qF(a)(r)(n),loopBuf:jF(a)(r)(n),lowshelf:QF(a)(r)(n),lowpass:XF(a)(r)(n),notch:YF(a)(r)(n),playBuf:aE(a)(r)(n),peaking:ZF(a)(r)(n),microphone:KF(a)(r)(n),pan:gE(a)(r)(n),periodicOsc:uE(a)(r)(n),recorder:AE(a)(r)(n),sawtoothOsc:kE(a)(r)(n),sinOsc:dE(a)(r)(n),squareOsc:yE(a)(r)(n),triangleOsc:SE(a)(r)(n),waveShaper:CE(a)(r)(n),next:a8(n)(u)})}}}};var ky=function(){function t(){}return t.value=new t,t}(),TE={attr:function(t){return function(r){return c({key:"checked",value:s(r)})}}};var di=function(){function t(){}return t.value=new t,t}();var Zi={attr:function(t){return function(r){return c({key:"type",value:s(r)})}}};var yi=O("input");var l8=fa(gi);var p8=function(t){return t},Um=function(t){var r=Ao(t),e=t.Alternative0(),n=ht(e.Plus1().Alt0()),u=M(e.Applicative0());return function(a){return function(o){return r(n(u(a))(o))}}};var ov=function(t){return function(r){return t(r)}},Vl=function(t){var r=S(t);return{map:function(e){return function(n){return function(u){return n(r(function(a){return function(o){return a(e(o))}})(u))}}}}},No=function(t){var r=S(Vl(t)),e=S(t);return function(n){return function(u){return function(a){return ov(r(n)(u))(e(Jf)(a))}}}};var Bm=function(t){return No(t)(w)};var Bu=p8;var FE=function(t){var r=ka(t),e=t.Alternative0(),n=ht(e.Plus1().Alt0()),u=M(e.Applicative0()),a=S(t.Filterable1().Functor1());return function(o){return function(i){return Bu(function(f){return r(n(u(ov(o)(f)))(a(function(m){return ov(m)(f)})(i)))})}}},EE=function(t){var r=S(t),e=Vl(t);return{apply:function(n){return function(u){return function(a){return u(n(r(l8)(a)))}}},Functor0:function(){return e}}};var ME=lt(Qn),OE=Pn(Wt),_8=Xo(za),wE=Dn(Wt)(Me),PE=rr(or);var Gl=function(t){return function(r){return Cn(function(e){return we(r)(function(n){return function(){var a=G_(t)();return e({acTime:a,value:n})()}})})}};var IE=function(t){return function(r){return function(e){var n=function(u){return function(a){return function(o){return function(i){return function(f){return function(m){return function(){var D=Ur(o)();return OE(D)(function(){var b=G_(t)(),_=Zv(u$(_8(a-b-.04)(.01)*1e3))(function(){var G=Ur(o)();return OE(G)(function(){return te(a)(f)(),u(a)(),n(u)(a+m)(o)(i)(f)(m)()})()})();return te(new F(_))(i)()})()}}}}}}};return Cn(function(u){return function(){var o=Or(!0)(),i=Or(P.value)(),f=G_(t)(),m=Or(f+r)();n(u)(r)(o)(i)(m)(r)();var v=we(e)(function(D){return function(){ME(Ur(i))(wE(zp))();var b=Ur(m)();return n(u)(b+D)(o)(i)(m)(D)()}})();return PE(PE(v)(te(!1)(o)))(ME(Ur(i))(wE(zp)))}})}}};var cu=function(t){return function(r){return function(e){return function(n){return function(u){var a=e===t||n===r;if(a)return r;var o=(n-r)/(e-t),i=r-o*t;return o*u+i}}}}};var RE=q(),v8=Dt({reflectType:function(){return`<section>
  <h2>Fold</h2>

  <p>The type of <code>fold</code> is:</p>

  <pre><code>fold
    :: forall event a b
    . IsEvent event
    => (a -> b -> b)
    -> event a
    -> b
    -> event b</code></pre>

  <p>Fold starts with some initial state <code>b</code> and, based on incoming events, allows you to change the state.</p>

  <p>One way <code>fold</code> is useful is to retain when certain actions happen. In the following example, we use <code>requestAnimationFrame</code> to animate the audio and we use four <code>fold</code>-s to store the ambitus and velocity of both vibrato and tremolo.</p>

  <pre><code>~txt~</code></pre>

  ~empl~

  <p><code>fold</code> is so powerful because it allows us to localize state to <i>any</i> event. In the example above, instead of having a global state, our two folds allow for two <i>ad hoc</i> local states.</p>

</section>`}})()()(RE(RE(N)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}})),s8=Ii()(Mi({reflectSymbol:function(){return"cbx"}})()()()(De({reflectSymbol:function(){return"cbx0"}})()()(De({reflectSymbol:function(){return"cbx1"}})()()(De({reflectSymbol:function(){return"cbx2"}})()()(De({reflectSymbol:function(){return"cbx3"}})()()(Yn)()())()())()())()())(Mi({reflectSymbol:function(){return"startStop"}})()()()(De({reflectSymbol:function(){return"start"}})()()(De({reflectSymbol:function(){return"stop"}})()()(Yn)()())()())(Yn)()())()()),LE=ht(qt),Hm=M(X),m8=Um(me),D8=da(me),NE=_u(Ht)(h),Rf=S(ct),UE=mt(ke),zm=oe(ct),b8=it(pn),Jl=W(ct),A8=K(Z),BE=M(Wt),k8=Mn(ne),d8=Na(ne),y8=Qc(me),g8=Er(Jn),h8=No(ct),WE=Ao(me),qE=Bm(ct),S8=mu(_t),x8=Du()(Pa),C8=Il(Ol(Zt(Cr()(dt(dt(xr)(Ml($l(fi)))()()()({reflectSymbol:function(){return"spec"}}))(Fl)()()()({reflectSymbol:function(){return"frequency"}})))(Sr()()))),HE=Pe(Se(fi)()(ta)()(Po)),zE=Pe(Se(Mo)()(ra)()(ta)),VE=Pe(Se(Oo)()(ea)()(ra)),GE=Pe(Se(wo)()(Io)()(ea)),dy=rn(Ht)(h),$8=Q(),T8=wf()(Pa),fv=rr(or),F8=mn(ne),E8=mt(Ar),M8=S(ir),O8=mt(Zi),w8=mt(TE),P8=CD(ir),I8=function(){return d.value}(),JE=function(t){return function(r){return function(e){return function(n){return v8(I8)({txt:C(`module Main where

import Prelude

import Control.Alt ((<|>))
import QualifiedDo.OneOfMap as O
import QualifiedDo.Alt as OneOf
import Data.Tuple.Nested ((/\\))
import Data.Vec ((+>))
import Data.Vec as V
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (text)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import FRP.Behavior (sampleBy, sample_, step)
import FRP.Event (memoize)
import FRP.Event.Animate (animationFrameEvent)
import FRP.Event.Class (fold, mapAccum, sampleOnRight)
import FRP.Event.VBus (V, vbus)
import Data.Number (pi, sin)
import Type.Proxy (Proxy(..))
import Ocarina.Clock(withACTime)
import Ocarina.Control (gain, periodicOsc)
import Ocarina.Interpret (close, constant0Hack, context)
import Ocarina.Math (calcSlope)
import Ocarina.Core (AudioNumeric(..), _linear, bangOn)
import Ocarina.Properties as P
import Ocarina.Run (run2e)

type Cbx = V (cbx0 :: Unit, cbx1 :: Unit, cbx2 :: Unit, cbx3 :: Unit)

type StartStop = V (start :: Unit, stop :: Effect Unit)

type UIEvents = V
  ( startStop :: StartStop
  , cbx :: Cbx
  )

main :: Effect Unit
main = runInBody1
  ( vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        startE = pure unit <|> event.startStop.start
        stopE = event.startStop.stop
        chkState e = step false $ fold (const not) e false
        cbx0 = chkState event.cbx.cbx0
        cbx1 = chkState event.cbx.cbx1
        cbx2 = chkState event.cbx.cbx2
        cbx3 = chkState event.cbx.cbx3
      D.div_
        [ D.button
            ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                stopE <#> (_ *> push.startStop.start unit)
                startE $> do
                  ctx <- context
                  c0h <- constant0Hack ctx
                  let
                    cevt fast b tm = mapAccum
                      ( \\(oo /\\ act) (pact /\\ pt) ->
                          let
                            tn = pt +
                              ( (act - pact) *
                                  (if oo then fast else 1.0)
                              )
                          in
                            ((act /\\ tn) /\\ tn)
                      )
                      (sampleBy (/\\) b tm)
                      (0.0 /\\ 0.0)

                  r <- run2e ctx
                    ( memoize
                        ( map (add 0.04 <<< _.acTime)
                            $ withACTime ctx animationFrameEvent
                        )
                        \\acTime ->
                          let
                            ev0 = cevt 8.0 cbx0 acTime
                            ev1 = map (if _ then 4.0 else 1.0) $ sample_ cbx1 acTime
                            ev2 = cevt 4.0 cbx2 acTime
                            ev3 = map (if _ then 4.0 else 1.0) $ sample_ cbx3 acTime
                            evs f a = sampleOnRight acTime
                              $ map ($)
                              $ sampleOnRight a
                              $ { f: _, a: _, t: _ } <$> f
                          in
                            [ gain 0.0
                                ( evs ev0 ev1 <#> \\{ f, a, t } -> P.gain $ AudioNumeric
                                    { n: calcSlope 1.0 0.01 4.0 0.15 a * sin (pi * f) + 0.15
                                    , o: t
                                    , t: _linear
                                    }
                                )
                                [ periodicOsc
                                    { frequency: 325.6
                                    , spec: (0.3 +> -0.1 +> 0.7 +> -0.4 +> V.empty)
                                        /\\ (0.6 +> 0.3 +> 0.2 +> 0.0 +> V.empty)
                                    }
                                    ( OneOf.do
                                        bangOn
                                        evs ev2 ev3 <#> \\{ f, a, t } -> P.frequency
                                          $ AudioNumeric
                                              { n: 325.6 +
                                                  (calcSlope 1.0 3.0 4.0 15.5 a * sin (pi * f))
                                              , o: t
                                              , t: _linear
                                              }
                                    )
                                ]
                            ]
                    )
                  push.startStop.stop (r *> c0h *> close ctx)
            )
            [ text OneOf.do
                startE $> "Turn on"
                stopE $> "Turn off"
            ]
        , D.div
            ( O.oneOfMap (map (attr D.Style)) O.do
                stopE $> "display:block;"
                startE $> "display:none;"
            )
            ( map
                ( \\e -> D.input
                    ( OneOf.do
                        pure (D.Xtype := "checkbox")
                        pure (D.OnClick := cb (const (e unit)))
                        startE $> (D.Checked := "false")
                    )
                    []
                )
                ([ _.cbx0, _.cbx1, _.cbx2, _.cbx3 ] <@> push.cbx)
            )
        ]
  )`),empl:s8(d.value)(function(u){return function(a){var o=LE(Hm(void 0))(a.startStop.start),i=function(A){return m8(!1)(D8(function(b){return function(_){return!b}})(!1)(A))},f=i(a.cbx.cbx3),m=i(a.cbx.cbx2),v=i(a.cbx.cbx1),D=i(a.cbx.cbx0);return Lr([on(NE(Rf(function(){var A=UE(ee.value);return function(b){return A(Vr(w(b)))}}()))([zm(b8(Jl(o)(A8))(LE(Hm(BE(void 0)))(Rf(function(A){return A.value0})(n))))(function(A){return function(){A();var _=k8(),k=d8(_)(),G=function(vr){return function(Gr){return function(z){return y8(function(zt){return function(st){var qr=zt.value1+(st.value1-zt.value0)*function(){return st.value0?vr:1}();return new U(new U(st.value1,qr),qr)}})(new U(0,0))(h8(U.create)(Gr)(z))}}},nt=Ul(_)(ti(Rf(function(){var vr=g8(.04);return function(Gr){return vr(function(z){return z.acTime}(Gr))}}())(Gl(_)(xf)))(function(vr){var Gr=function(Wr){return function(fn){return WE(vr)(Rf(Mc)(WE(fn)(Rf(function(On){return function(ia){return function(be){return{f:On,a:ia,t:be}}}})(Wr))))}},z=Rf(function(Wr){return Wr?4:1})(qE(f)(vr)),zt=G(4)(m)(vr),st=Rf(function(Wr){return Wr?4:1})(qE(v)(vr)),qr=G(8)(D)(vr);return[S8(0)(zm(Gr(qr)(st))(function(Wr){return x8({n:cu(1)(.01)(4)(.15)(Wr.a)*Is(sf*Wr.f)+.15,o:Wr.t,t:Ji})}))([C8({frequency:325.6,spec:new U(HE(.3)(zE(-.1)(VE(.7)(GE(-.4)(Oa)))),HE(.6)(zE(.3)(VE(.2)(GE(0)(Oa)))))})(dy([$8,zm(Gr(zt)(z))(function(Wr){return T8({n:325.6+cu(1)(3)(4)(15.5)(Wr.a)*Is(sf*Wr.f),o:Wr.t,t:Ji})})]))])]}))(),bt=fv(fv(nt)(k))(F8(_));return t(fv(bt)(u.startStop.start(void 0)))(),u.startStop.stop(bt)()}}),zm(a.startStop.stop)(function(A){return fv(A)(fv(t(BE(void 0)))(u.startStop.start(void 0)))})]))([Ve(dy([Jl(o)("Turn on"),Jl(a.startStop.stop)("Turn off")]))]),sr(NE(Rf(E8(Ot.value)))([Jl(a.startStop.stop)("display:block;"),Jl(o)("display:none;")]))(M8(function(A){return yi(dy([Hm(O8(di.value)("checkbox")),Hm(UE(ee.value)(Vr(w(A(void 0))))),Jl(o)(w8(ky.value)("false"))]))([])})(P8([function(A){return A.cbx0},function(A){return A.cbx1},function(A){return A.cbx2},function(A){return A.cbx3}])(u.cbx)))])}})})}}}};var jE={recip:function(t){return 1/t},Ring0:function(){return jf}};var XE=function(t){return function(r){return{EuclideanRing0:function(){return t},DivisionRing1:function(){return r}}}};function jl(t){return function(){return function(r){return t(r)()}}}function Xl(t){return function(r){return function(e){return function(n){return function(){return n.addEventListener(t,r,e)}}}}}function Ql(t){return function(r){return function(e){return function(n){return function(){return n.removeEventListener(t,r,e)}}}}}function yy(t){return t.clientX}function gy(t){return t.clientY}function cv(t){return t.button}var lv=kt("MouseEvent");var QE=S(L),Vm=Dn(Wt)(Me),Lf=po();var q8=vS(Te),H8=Yv(Te);var KE=function(t){return function(r){return Cn(function(e){return we(r)(function(n){return function(){var a=Ur(t.buttons)();return e({value:n,buttons:a})()}})})}};var YE=function(){var r=Or(P.value)(),e=Or(Fb)(),n=QE(GA)(Fo)(),u=jl(function(f){return Vm(function(m){return te(new F({x:yy(m),y:gy(m)}))(r)})(lv(f))})(),a=jl(function(f){return Vm(function(m){return Yf(q8(cv(m)))(e)})(lv(f))})(),o=jl(function(f){return Vm(function(m){return Yf(H8(cv(m)))(e)})(lv(f))})();Xl(Lf("mousemove"))(u)(!1)(n)(),Xl(Lf("mousedown"))(a)(!1)(n)(),Xl(Lf("mouseup"))(o)(!1)(n)();var i=function(){return Ql(Lf("mousemove"))(u)(!1)(n)(),Ql(Lf("mousedown"))(a)(!1)(n)(),Ql(Lf("mouseup"))(o)(!1)(n)()};return{position:r,buttons:e,dispose:i}},ZE=Cn(function(t){return function(){var e=QE(GA)(Fo)(),n=jl(function(u){return Vm(function(a){return t(cv(a))})(lv(u))})();return Xl(Lf("mousedown"))(n)(!1)(e)(),Ql(Lf("mousedown"))(n)(!1)(e)}});var z8=S(ct);var rM=function(t){return Bu(function(r){return z8(function(e){return e.value(e.buttons)})(KE(t)(r))})};var xy=function(t){return t};function Gm(){return Date.now()}var $M=function(t){return Cn(function(r){return we(t)(function(e){return function(){var u=Gm();return r({time:u,value:e})()}})})};var y6=S(ct),g6=Bu(function(t){return y6(function(r){return r.value(r.time)})($M(t))}),$y=S(Vl(ct))(function(){var t=vl(lC);return function(r){return t(xy(r))}}())(g6);var My=K(Z),S6=Ao(me),x6=Xc(me),C6=Um(me),PM=Bm(ct),$6=XE(Ev)(jE),Ty=S(Vl(ct)),TM=ie(),FM=it(EE(ct)),T6=FE(me),Ym=W(ct),EM=q(),F6=Dt({reflectType:function(){return`<section>
  <h2>Fix</h2>

  <p>Fix, like it's equivalent in ocarina that we've already seen, creates a feedback loop. However, in this case, we are talking about a feedback loop of <i>events</i>, not sound.</p>

  <p>At first glance, it may not be clear why we need an event stream to feed back into itself? It seems prone to saturation: if you have a counter that feeds back into itself with a delay, after a few seconds you'll have so many events that it will crash your browser (I've tried it!).</p>

  <p>However, there's one important circumstance where you need fixed points: when an event can only be defined in terms of itself. One classic category of this is the <i>differential equation</i>. Differential equations allow you to produce <a href="https://en.wikipedia.org/wiki/Simple_harmonic_motion">Slinky effects, aka simple harmonic motion,</a> and a lot of other neat behaviors that are difficult to produce via other means.</p>

  <p>Let's listen to the sound of simple harmonic motion in the example below, courtesy of <code>fix</code>. The differential equation in the example below comes from Phil Freeman, the creator of the PureScript language and the author of the <code>purescript-behaviors</code> package. When you click "Turn on", you won't hear much, but press and release your mouse anywhere on the screen to hear the differential equation take flight!</p>

  <pre><code>~txt~</code></pre>

  ~empl~

  <p>When working with stateful events, a good way to decide if you should use <code>fold</code> versus <code>fix</code> is to ask the following question: can I incrementally change my state based on an initial state, or is my state defined in terms of how it changes? If you can incrementally change your state, go with <code>fold</code>. If, on the other hand, your state is defined in terms of how it changes, go with <code>fix</code>.</p>
</section>`}})()()(EM(EM(N)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}})),E6=Ii()(De({reflectSymbol:function(){return"start"}})()()(De({reflectSymbol:function(){return"stop"}})()()(Yn)()())()()),Kl=ht(qt),MM=M(X),M6=_u(Ht)(h),Ua=S(ct),O6=mt(ke),OM=oe(ct),w6=it(pn),wM=M(Wt),P6=Mn(ne),I6=Na(ne),Fy=S(x_),R6=sc($$),Jm=lt(Gs),L6=M(C_),N6=Pe(Se(fi)()(ta)()(Po)),U6=Pe(Se(Mo)()(ra)()(ta)),B6=Pe(Se(Oo)()(ea)()(ra)),W6=Pe(Se(wo)()(Io)()(ea)),jm=it(Js),pv=mu(_t),_v=Du()(Pa),Uo=xl(Af),vv=Xo(za),Oy=Cr(),wy=dt(xr),IM={reflectSymbol:function(){return"q"}},Py={reflectSymbol:function(){return"frequency"}},Iy=Sr()(),q6=Ll(Mk(Zt(Oy(dt(wy(pT)()()()(IM))(kk)()()()(Py)))(Iy))),H6=KT(Tl),Ey=Xi(ki(Zt(Oy(dt(wy(ji)()()()(IM))(bi)()()()(Py)))(Iy))),Xm=Il(Ol(Zt(Oy(dt(wy(Ml($l(fi)))()()()({reflectSymbol:function(){return"spec"}}))(Fl)()()()(Py)))(Iy))),Qm=Q(),Km=wf()(Pa),sv=rr(or),z6=mn(ne),V6=rn(Ht)(h),G6=function(t){var r=function(o){var i=o.Filterable1().Functor1(),f=W(i),m=rs(o),v=No(i),D=da(o),A=Ao(o);return function(b){var _=b.DivisionRing1().Ring0(),k=_.Semiring0(),G=Er(k),nt=In(k),bt=Ja(b.EuclideanRing0()),vr=Ye(k),Gr=du(_);return function(z){var zt=Er(z);return function(st){return function(qr){return function(Wr){return function(fn){var On=G(nt)(nt),ia=function(be){return function(ue){if(be.last instanceof P)return ue;if(be.last instanceof F)return zt(ue)(st(function(Wu){return bt(vr(Wu(zt(be.last.value0.value1)(be.now.value1)))(Gr(be.now.value0)(be.last.value0.value0)))(On)}));throw new Error("Failed pattern match at Ocarina.Example.Docs.FixEx (line 106, column 5 - line 106, column 35): "+[be.constructor.name,ue.constructor.name])}};return Bu(function(be){var ue=ov(fn)(f(be)(My)),Wu=m(v(U.create)(Wr)(ue)),to=D(Jt(ia))(qr)(Wu);return A(to)(be)})}}}}}}},e=function(o){var i=r(o);return function(f){return i(f)(f.DivisionRing1().Ring0().Semiring0())(function(m){return m(My)})}},n=e(me)($6),u=function(o){return function(i){return Bu(function(f){return S6(x6(function(m){var v=i(C6(o)(m));return PM(v)(f)}))(f)})}},a=function(o){return function(i){return function(f){if(_S(o))return-8*(i-1)-f*2;if(Jr)return 2*(4-i);throw new Error("Failed pattern match at Ocarina.Example.Docs.FixEx (line 62, column 3 - line 64, column 34): "+[o.constructor.name,i.constructor.name,f.constructor.name])}}};return u(2)(function(o){return n(2)(Ty(TM)($y))(function(){var i=u(10)(function(f){return n(10)(Ty(TM)($y))(FM(FM(Ty(a)(rM(t)))(o))(f))});return T6(i)(Ym(ZE)(i))}())})},J6=function(){return d.value}(),RM=function(t){return function(r){return function(e){return function(n){return F6(J6)({txt:C(`module Main

import Prelude

import Control.Alt ((<|>))
import QualifiedDo.OneOfMap as O
import QualifiedDo.Alt as OneOf
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong (second)
import Data.Set (isEmpty)
import Data.Tuple.Nested ((/\\))
import Data.Vec ((+>))
import Data.Vec as V
import Deku.Attribute (attr, cb)
import Deku.Control (text)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import Effect.Random (randomInt)
import FRP.Behavior (ABehavior, Behavior, behavior, sample, sampleBy, sample_, step, switcher)
import FRP.Behavior.Mouse (buttons)
import FRP.Behavior.Time as Time
import FRP.Event (memoize)
import FRP.Event.Animate (animationFrameEvent)
import FRP.Event.Class (class IsEvent, fix, fold, sampleOnRight, withLast)
import FRP.Event.Mouse (Mouse, down, getMouse)
import FRP.Event.VBus (V, vbus)
import Test.QuickCheck (arbitrary, mkSeed)
import Test.QuickCheck.Gen (evalGen)
import Type.Proxy (Proxy(..))
import Ocarina.Clock(withACTime)
import Ocarina.Control (bandpass_, gain, lowpass_, periodicOsc, squareOsc_)
import Ocarina.Interpret (close, constant0Hack, context)
import Ocarina.Core (AudioNumeric(..), _linear, bangOn)
import Ocarina.Properties as P
import Ocarina.Run (run2e)

type StartStop = V (start :: Unit, stop :: Effect Unit)

-- \`swell\` is an interactive function of time defined by a differential equation:
--
-- d^2s/dt^2
--   | mouse down = \u237A - \u03B2s
--   | mouse up   = \u0263 - \u03B4s - \u03B5 ds/dt
--
-- So the function exhibits either decay or growth depending on if
-- the mouse is pressed or not.
--
-- We can solve the differential equation by integration using \`solve2'\`.
swell :: Mouse -> Behavior Number
swell mouse =
  fixB 2.0 \\b ->
    integral' 2.0 (unwrap <$> Time.seconds)
      let
        db = fixB 10.0 \\db_ ->
          integral' 10.0 (unwrap <$> Time.seconds) (f <$> buttons mouse <*> b <*> db_)
      in
        switcher db (down $> db)
  where
  f bs s ds
    | isEmpty bs = -8.0 * (s - 1.0) - ds * 2.0
    | otherwise = 2.0 * (4.0 - s)

  fixB :: forall a. a -> (Behavior a -> Behavior a) -> Behavior a
  fixB a fn = behavior \\s ->
    fix \\event ->
      let
        b = fn (step a event)
      in
        { input: sample_ b s, output: sampleOnRight event s }

  -- | Integrate with respect to some measure of time.
  -- |
  -- | This function approximates the integral using the trapezium rule at the
  -- | implicit sampling interval.
  -- |
  -- | The \`Semiring\` \`a\` should be a vector field over the field \`t\`. To represent
  -- | this, the user should provide a _grate_ which lifts a multiplication
  -- | function on \`t\` to a function on \`a\`. Simple examples where \`t ~ a\` can use
  -- | the \`integral'\` function instead.
  integral
    :: forall event a t
     . IsEvent event
    => Field t
    => Semiring a
    => (((a -> t) -> t) -> a)
    -> a
    -> ABehavior event t
    -> ABehavior event a
    -> ABehavior event a
  integral g initial t b =
    behavior \\e ->
      let
        x = sample b (e $> identity)
        y = withLast (sampleBy (/\\) t x)
        z = fold approx y initial
      in
        sampleOnRight z e
    where
    approx { last: Nothing } s = s
    approx { now: t1 /\\ a1, last: Just (t0 /\\ a0) } s = s + g (\\z -> z (a0 + a1) * (t1 - t0) / two)

    two :: t
    two = one + one

  -- | Integrate with respect to some measure of time.
  -- |
  -- | This function is a simpler version of \`integral\` where the function being
  -- | integrated takes values in the same field used to represent time.
  integral'
    :: forall event t
     . IsEvent event
    => Field t
    => t
    -> ABehavior event t
    -> ABehavior event t
    -> ABehavior event t
  integral' = integral (_ $ identity)

main :: Effect Unit
main = runInBody1
  ( vbus (Proxy :: _ StartStop) \\push event -> do
      let
        startE = pure unit <|> event.start
        stopE = event.stop
      D.div_
        [ D.button
            ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                event.stop <#> (_ *> push.start unit)
                startE $>
                  do
                    ctx <- context
                    c0h <- constant0Hack ctx
                    mouse <- getMouse
                    ri <- randomInt 0 10000
                    let
                      ttap (o /\\ n) = AudioNumeric { o: o + 0.04, n, t: _linear }
                      fund = 90.4
                      spcn = map (_ - 0.5) arbitrary
                      spc' = do
                        a <- spcn
                        b <- spcn
                        c <- spcn
                        d <- spcn
                        pure (a +> b +> c +> d +> V.empty)
                      spc = (/\\) <$> spc' <*> spc'
                      spcs = { s0: _, s1: _, s2: _, s3: _ } <$> spc <*> spc <*> spc <*> spc
                      allSpcs = evalGen spcs { newSeed: mkSeed ri, size: 5 }
                    r <- run2e ctx
                      ( memoize
                          ( map (\\{ acTime, value } -> acTime /\\ value)
                              $ withACTime ctx
                              $ sample_ (swell mouse) animationFrameEvent
                          )
                          \\swm ->
                            [ gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\\x -> max (-0.4) $ 0.5 * (x - 1.0)) <$> swm
                                )
                                [ lowpass_ { frequency: fund, q: 20.0 }
                                    [ squareOsc_ fund ]
                                ]
                            , gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\\x -> max (-0.2) $ 0.4 * (x - 3.0)) <$> swm
                                )
                                [ bandpass_ { frequency: fund * 4.0, q: 20.0 }
                                    [ periodicOsc
                                        { frequency: (fund * 3.02)
                                        , spec: allSpcs.s0
                                        }
                                        ( bangOn <|>
                                            ( P.frequency
                                                <<< ttap
                                                <<< second (\\x -> fund * 3.02 + 14.0 * (x - 1.0)) <$> swm
                                            )
                                        )
                                    ]
                                ]
                            , gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\\x -> max (-0.1) $ 0.2 * (x - 6.0)) <$> swm
                                )
                                [ bandpass_ { frequency: fund * 6.0, q: 20.0 }
                                    [ periodicOsc
                                        { frequency: fund * 5.07
                                        , spec: allSpcs.s1
                                        }
                                        ( bangOn <|>
                                            ( P.frequency
                                                <<< ttap
                                                <<< second (\\x -> fund * 5.07 + 18.0 * (x - 1.0)) <$> swm
                                            )
                                        )
                                    ]
                                ]
                            , gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\\x -> max 0.0 $ 0.2 * (x - 3.0)) <$> swm
                                )
                                [ bandpass_ { frequency: fund * 8.0, q: 20.0 }
                                    [ periodicOsc
                                        { frequency: fund * 7.13
                                        , spec: allSpcs.s2
                                        }
                                        ( bangOn <|>
                                            ( P.frequency
                                                <<< ttap
                                                <<< second (\\x -> fund * 7.13 + 32.0 * (x - 1.0)) <$> swm
                                            )
                                        )
                                    ]
                                ]
                            , gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\\x -> max 0.0 $ 0.1 * (x - 7.0)) <$> swm
                                )
                                [ periodicOsc
                                    { frequency: fund * 9.14
                                    , spec: allSpcs.s3
                                    }
                                    ( bangOn <|>
                                        ( P.frequency
                                            <<< ttap
                                            <<< second (\\x -> fund * 9.14 + 31.0 * (x - 1.0)) <$> swm
                                        )
                                    )
                                ]
                            ]
                      )
                    push.stop (r *> c0h *> close ctx)
            )
            [ text $ OneOf.do
                startE $> "Turn on"
                stopE $> "Turn off"
            ]
        ]
  )`),empl:E6(d.value)(function(u){return function(a){var o=Kl(MM(void 0))(a.start);return Lr([on(M6(Ua(function(){var i=O6(ee.value);return function(f){return i(Vr(w(f)))}}()))([OM(w6(Ym(o)(My))(Kl(MM(wM(void 0)))(Ua(function(i){return i.value0})(n))))(function(i){return function(){i();var m=P6(),v=I6(m)(),D=YE(),A=y_(0)(1e4)(),b=function(z){return{o:z.value0+.04,n:z.value1,t:Ji}},_=Fy(function(z){return z-.5})(R6),k=Jm(_)(function(z){return Jm(_)(function(zt){return Jm(_)(function(st){return Jm(_)(function(qr){return L6(N6(z)(U6(zt)(B6(st)(W6(qr)(Oa)))))})})})}),G=jm(Fy(U.create)(k))(k),nt=jm(jm(jm(Fy(function(z){return function(zt){return function(st){return function(qr){return{s0:z,s1:zt,s2:st,s3:qr}}}}})(G))(G))(G))(G),bt=bf(nt)({newSeed:Df(A),size:5}),vr=Ul(m)(ti(Ua(function(z){return new U(z.acTime,z.value)})(Gl(m)(PM(G6(D))(xf))))(function(z){return[pv(0)(Ua(function(){var zt=Uo(function(st){return vv(-.4)(.5*(st-1))});return function(st){return _v(b(zt(st)))}}())(z))([q6({frequency:90.4,q:20})([H6(90.4)])]),pv(0)(Ua(function(){var zt=Uo(function(st){return vv(-.2)(.4*(st-3))});return function(st){return _v(b(zt(st)))}}())(z))([Ey({frequency:90.4*4,q:20})([Xm({frequency:90.4*3.02,spec:bt.s0})(Kl(Qm)(Ua(function(){var zt=Uo(function(st){return 273.00800000000004+14*(st-1)});return function(st){return Km(b(zt(st)))}}())(z)))])]),pv(0)(Ua(function(){var zt=Uo(function(st){return vv(-.1)(.2*(st-6))});return function(st){return _v(b(zt(st)))}}())(z))([Ey({frequency:90.4*6,q:20})([Xm({frequency:90.4*5.07,spec:bt.s1})(Kl(Qm)(Ua(function(){var zt=Uo(function(st){return 458.32800000000003+18*(st-1)});return function(st){return Km(b(zt(st)))}}())(z)))])]),pv(0)(Ua(function(){var zt=Uo(function(st){return vv(0)(.2*(st-3))});return function(st){return _v(b(zt(st)))}}())(z))([Ey({frequency:90.4*8,q:20})([Xm({frequency:90.4*7.13,spec:bt.s2})(Kl(Qm)(Ua(function(){var zt=Uo(function(st){return 644.552+32*(st-1)});return function(st){return Km(b(zt(st)))}}())(z)))])]),pv(0)(Ua(function(){var zt=Uo(function(st){return vv(0)(.1*(st-7))});return function(st){return _v(b(zt(st)))}}())(z))([Xm({frequency:90.4*9.14,spec:bt.s3})(Kl(Qm)(Ua(function(){var zt=Uo(function(st){return 826.2560000000001+31*(st-1)});return function(st){return Km(b(zt(st)))}}())(z)))])]}))(),Gr=sv(sv(vr)(v))(z6(m));return t(sv(Gr)(u.start(void 0)))(),u.stop(Gr)()}}),OM(a.stop)(function(i){return sv(i)(sv(t(wM(void 0)))(u.start(void 0)))})]))([Ve(V6([Ym(o)("Turn on"),Ym(a.stop)("Turn off")]))])])}})})}}}};var LM=q(),X6=Dt({reflectType:function(){return`<div>
  <h1>State</h1>

  <h3>Or Events 2.0</h3>
  <p>
    The name of this section is a bit of a nisnomer. While it will address the issue of maintaining state in an audio graph, it's really just about two mechanisms you can use to make an <code>Event</code> stateful. One is called <code>fold</code>, and the other is called <code>fix</code>. Both are part of the <code>IsEvent</code> typeclass, which means you get them for free when working with events.
  </p>

  ~fold~
  ~fix~

  <h2>Next steps</h2>
  <p>Using <code>fold</code> and <code>fix</code>, we can create internal state in our Web Audio works that would be really tedious and error-prone to achieve in vanilla JS or other compile-to-JS languages. There's still one nagging issue that we haven't addressed, though. For all of the flexibility we can achieve with events, we still can't flex the audio graph itself, meaning that we can't add or remove components. In the next section, we'll learn how to do that with <a ~next~ style="cursor:pointer;">subgraphs</a>.</p>
</div>`}})()()(LM(LM(Je()(N)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"fold"}})({reflectSymbol:function(){return"fold"}}))({reflectType:function(){return"fix"}})({reflectSymbol:function(){return"fix"}})),Q6=ou(qt)(X),K6=rr(or),Y6=function(){return d.value}(),NM=function(t){return function(r){return function(e){return function(n){var u=fu(t)(e);return X6(Y6)({next:Q6(n)(K6(r(z_.value))(Xe)),fold:JE(u)(r)(e)(n),fix:RM(u)(r)(e)(n)})}}}};var zM=Ju(Wn),Ny=ht(qt),Uy=M(X),tJ=Du()(En),rJ=mu(_t),eJ=gc(gf),UM=q(),nJ=Yt({reflectType:function(){return`<section>
  <h2>Example 1: Hello events</h2>

  <p>Let's say hi to events! The simplest of events, which we've seen already, are the ones that occur <span style="font-weight:800;">now</span>, that is to say, immediately upon subscription. You create those types of events using <code>bang</code>. In this section, we'll use <code>bang</code> to set several different types of values:</p>

  <ul>
    <li><code>AudioEnvelope</code> to create an envelope for the gain node. To construct one, use a record with the following parameters:<ul><li><code>p</code>: a list of numbers that will be interpolated over.</li><li><code>o</code>: the offset in time from the AudioContext clock's start time.</li><li><code>d</code>: the duration of the envelope.</li></ul></li>
    <li><code>AudioOnOff</code> to turn the sine-wave oscillator on and off. To construct one, use a record with the following parameters:<ul><li><code>n</code>: an enum with the value <code>_on</code>, <code>_off</code> or <code>_onOff</code> (more on this in <a href="#example3">Example 3</a> below).</li><li><code>o</code>: the offset in time from the AudioContext clock's start time.</li></ul></li>
  </ul>

  <p>After that, in the example below, it's functions all the way down. <code>oon</code> and <code>oof</code> create our on/off events, <code>env</code> creates our gain envelope, <code>ooo'</code> and <code>env'</code> specialize these envelopes to a specific point in time, and <code>cell</code> creates a single cell that we deploy 100 times.</p>

  <p>One important thing to note here is the use of the tie fighter (<code>&lt;|&gt;</code>), aka <code>alt</code>, in the definition of <code>ooo'</code>. The <code>Event</code> type, when <code>alt</code>'d, preserves a before-after relationship of the left and right operands when the operands happen at the same time. This is a bit hackish: the events conceptually happen at the same time, but on our CPU, one has to follow the other. We can use this, however, to make sure that certain events happen in a logical sequence. For example, an <code>off</code> instruction must be issued after an <code>on</code> instruction, which we guarantee by using <code>oon</code> on the left side of the alt. If we did it the other way, the <code>on</code> instruction would be last and we'd wind up with 100 oscillators playing at the same time!</p>

  <p>A last thing to note before the music plays is how scheduling works here. Even though all the events are issued upfront via <code>bang</code>, they schedule things to be played <i>later</i> in the audio context. We'll see more advanced scheduling techniques in the <a href="#example4"><code>requestAnimationFrame</code> example below</a>.</p>

  <pre><code>@txt@</code></pre>

  @ex0@

  <p>Unlike the previous examples, this one and all subsequent ones are "batteries included", meaning they are single-file, self-contained PureScript examples that you can compile and run yourself.</p>

</section>
`}})({reflectType:function(){return"@"}})()()(UM(UM(N)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}})),BM=oe(ct),uJ=it(pn),WM=S(ct),qM=M(Wt),aJ=mt(ke),Ry=rr(or),iJ=Pt(_t),oJ=qu(hi),fJ=S(ir),cJ=function(){function t(){}return t.value=new t,t}(),HM=function(){function t(){}return t.value=new t,t}(),Ly=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),lJ=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((..))
import Data.Int (toNumber)
import Data.Profunctor (lcmap)
import Deku.Attribute (cb, (:=))
import Deku.Control (text)
import Deku.DOM as D
import Deku.Toplevel (runInBody, runInBody1)
import Effect (Effect)
import FRP.Event (bus)

import Math (pow)
import Ocarina.Control (gain_, gain, sinOsc)
import Ocarina.Core (AudioEnvelope(..), AudioOnOff(..), _on, _off)
import Ocarina.Properties (onOff)
import Ocarina.Properties as P
import Ocarina.Run (run2_)

data UIEvents = Init | Start | Stop (Effect Unit)

-- an event to turn our oscillators on
oon o = pure $ onOff $ AudioOnOff { x: _on, o }
-- an event to turn our oscillators off
oof o = pure $ onOff $ AudioOnOff { x: _off, o }
-- an event with an envelope for our gain
env o = pure $ P.gain
  $ AudioEnvelope
      { p: [ 0.0, 0.4, 0.1, 0.05, 0.01, 0.0 ]
      , d: 0.8
      , o
      }

-- a single cell with four oscillators,
-- each of which have the envelope applied
cell = lcmap toNumber \\i -> do
  let
    ooo' x = oon (x + 0.27 * (i * (1.005 \`pow\` i)))
      <|> oof (x + 3.0 + 0.3 * (i * (1.005 \`pow\` i)))
    env' x = env (x + 0.3 * (i * (1.005 \`pow\` i)))
    strand x y =
      gain 0.0 (env' x) [ sinOsc (200.0 + i * y) (ooo' x) ]
  [ strand 0.2 4.0
  , strand 0.3 6.0
  , strand 0.45 14.0
  , strand 0.7 20.0
  ]

main :: Effect Unit
main = runInBody1 (bus \\push -> lcmap (pure Init <|> _) \\event ->
  D.div_
    [ D.button
        ( event <#>
            \\e -> D.OnClick := cb
              ( const $ case e of
                  Stop u -> u *> push Start
                  _ -> do
                    r <- run2_
                      [ gain_ 1.0
                          -- we create 100 cells
                          $ join
                          $ cell <$> 0 .. 100
                      ]
                    push $ Stop r
              )
        )
        [ text $ event <#> case _ of
            Stop _ -> "Turn off"
            _ -> "Turn on"
        ]
    ])
`;var pJ=function(){return d.value}(),_J=function(t){var r=M(t);return function(e){var n=Cc(e)(kc);return function(u){return r(n({x:bk,o:u}))}}},vJ=_J(X)(),sJ=function(t){var r=M(t);return function(e){var n=Cc(e)(kc);return function(u){return r(n({x:V0,o:u}))}}},mJ=sJ(X)(),DJ=zM(Br)(function(t){var r=function(u){return Ny(vJ(u+.27*(t*vf(1.005)(t))))(mJ(u+3+.3*(t*vf(1.005)(t))))},e=function(u){return Uy(tJ({p:[0,.4,.1,.05,.01,0],o:u+.3*(t*vf(1.005)(t)),d:.8}))},n=function(u){return function(a){return rJ(0)(e(u))([eJ(200+t*a)(r(u))])}};return[n(.2)(4),n(.3)(6),n(.45)(14),n(.7)(20)]}),VM=function(t){return function(r){return function(e){return nJ(d.value)(pJ)({txt:C(lJ),ex0:Ue(function(n){return zM(function(u){return Ny(Uy(cJ.value))(u)})(function(u){return Lr([on(BM(uJ(WM(U.create)(u))(Ny(Uy(qM(void 0)))(WM(function(a){return a.value0})(e))))(function(a){return aJ(ee.value)(Vr(w(function(){return a.value0 instanceof Ly?Ry(Ry(a.value0.value0)(n(HM.value)))(t(qM(void 0))):function(){a.value1();var i=wm([iJ(1)(oJ(fJ(DJ)(Ke(0)(100))))])();return t(Ry(i)(n(HM.value)))(),n(new Ly(i))()}}())))}))([Ve(BM(u)(function(a){return a instanceof Ly?"Turn off":"Turn on"}))])])})})})}}};var Bo=function(){function t(){}return t.value=new t,t}();var Nf={attr:function(t){return function(r){return c({key:"max",value:s(r)})}}};var Wo=function(){function t(){}return t.value=new t,t}();var Uf={attr:function(t){return function(r){return c({key:"min",value:s(r)})}}};var qo=function(){function t(){}return t.value=new t,t}();var Bf={attr:function(t){return function(r){return c({key:"input",value:J(r)})}}};var Ho=function(){function t(){}return t.value=new t,t}();var Wf={attr:function(t){return function(r){return c({key:"step",value:s(r)})}}};var zo=function(){function t(){}return t.value=new t,t}();var qf={attr:function(t){return function(r){return c({key:"value",value:s(r)})}}};var uD=function(t){var r=ht(t);return function(e){return function(n){return r(e)(n(void 0))}}};var AJ=WS,kJ=tt(AJ),Yl={convert:function(t){return t}},aD={convert:function(t){return t_(t)}},JM=function(t){return t},By=function(t){return t.convert},Tc=function(t){var r=By(t);return function(e){return function(n){return kJ(t_(e))(r(n(void 0)))}}};var dJ=_u(qS),iD=function(t){var r=By(t);return function(e){var n=dJ(e);return function(u){return function(a){return n(u)(JM(r(a)))}}}};function XM(t){return t.target}var Zl=function(t){return Be(XM(t))};var Hy=q(),hJ=Yt({reflectType:function(){return`<section>
 <h2>Example 2: Three sliders</h2>

  <p>In this example, we'll use three sliders to control the playback rate, the start time, and the end time of a looping buffer.</p>

  <p>There is a fair bit of DOM-related code in this example, so before showing the whole thing, let's isolate the Ocarina bit.</p>

  <pre><code>@wagtxt@</code></pre>

  <p>Note that our loopBuf consumes four events: in addition to the three sliders, there is a <code>bangOn</code> event that turns it on. For the events belonging to range sliders, we use <code>calcSlope</code> to normalize the range to sensible values for these parameters.</p>

  <p>Because each slider event contains a number, we can compose it with a function from <code>Ocarina.Properties</code>, like <code>playbackRate</code> or <code>loopStart</code>, to create an event that controls a Ocarina parameter. The <code>oneOf</code> directive indicates that the incoming event will be "one of" the events in the array. It's also possible to use the tie-fighter, aka <code>alt</code>, to separate each event, but I like the array syntax when possible as tie fighters do, after all, work for the Empire, and who likes the Empire?</p>

  <p>And below you'll find the full example. It also shows useful patterns like downloading audio files and filtering events.</p>

  <pre><code>@txt@</code></pre>

  @ex1@

</section>
`}})({reflectType:function(){return"@"}})()()(Hy(Hy(Hy(N)({reflectType:function(){return"wagtxt"}})({reflectSymbol:function(){return"wagtxt"}}))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})),SJ=Ii()(Mi({reflectSymbol:function(){return"slider"}})()()()(De({reflectSymbol:function(){return"s0"}})()()(De({reflectSymbol:function(){return"s1"}})()()(De({reflectSymbol:function(){return"s2"}})()()(Yn)()())()())()())(Mi({reflectSymbol:function(){return"startStop"}})()()()(De({reflectSymbol:function(){return"loading"}})()()(De({reflectSymbol:function(){return"start"}})()()(De({reflectSymbol:function(){return"stop"}})()()(Yn)()())()())()())(Yn)()())()()),zy=ht(qt),oD=M(X),xJ=tr(wl(Zt(Cr()(dt(dt(dt(dt(xr)(Dm)()()()({reflectSymbol:function(){return"playbackRate"}}))(mm)()()()({reflectSymbol:function(){return"loopStart"}}))(sm)()()()({reflectSymbol:function(){return"loopEnd"}}))(El)()()()({reflectSymbol:function(){return"buffer"}})))(Sr()()))),fD=uD(qt),CJ=Q(),Hf=S(ct),$J=Uu()(lm),TJ=PF(),FJ=IF(),QM=it(pn),EJ=Er(Jn),MJ=tt(He),OJ=S(ir),KM=iD(Yl)(h),mv=Tc(Yl),wJ=mt(Zi),PJ=mt(Uf),IJ=mt(Nf),RJ=mt(Wf),YM=Tc(aD),LJ=mt(qf),NJ=mt(Bf),UJ=Dn(Wt)(Me),BJ=Oc(Qn),WJ=cn(Nn),qJ=mt(ke),ZM=W(ct),Vy=M(Wt),tO=oe(ct),cD=rr(or),HJ=K(Z),Gy=lt(qn),zJ=Mn(Ge),VJ=Na(Ge),GJ=de(Ge),JJ=mn(ne),jJ=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (switcher, text, text_)
import Deku.Core (Nut)
import Bolson.Core (envy)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (create)
import FRP.Event.Class (biSampleOn)
import FRP.Event.VBus (V, vbus)
import QualifiedDo.Alt as OneOf
import QualifiedDo.OneOfMap as O
import Type.Proxy (Proxy(..))
import Ocarina.Control (loopBuf)
import Ocarina.Core (bangOn)
import Ocarina.Interpret (bracketCtx, decodeAudioDataFromUri)
import Ocarina.Math (calcSlope)
import Ocarina.Properties (loopEnd, loopStart, playbackRate)
import Ocarina.Run (run2_)
import Ocarina.WebAPI (BrowserAudioBuffer)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

type Slider = V (s0 :: Number, s1 :: Number, s2 :: Number)
type StartStop = V (start :: Unit, stop :: Effect Unit)
type UIEvents = V (startStop :: StartStop, slider :: Slider)

atari =
  "https://freesound.org/data/previews/100/100981_1234256-lq.mp3" :: String

main :: Effect Unit
main = do
  { push, event } <- create
  runInBody (switcher scene event)
  push Nothing
  launchAff_ $ bracketCtx
    \\ctx -> decodeAudioDataFromUri ctx atari >>= liftEffect
      <<< push
      <<< Just
  where
  scene
    :: forall payload
     . Maybe BrowserAudioBuffer
    -> Nut Effect payload
  scene = maybe (D.div_ [ text_ "Loading..." ]) \\buffer ->
    D.div_ $ pure $ envy $ vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        sl0 = event.slider.s0
        sl1 = event.slider.s1
        sl2 = event.slider.s2
        start = event.startStop.start <|> pure unit
        music = run2_
          [ loopBuf
              { buffer: buffer
              , playbackRate: 2.6
              , loopStart: 0.6
              , loopEnd: 1.1
              }
              OneOf.do
                bangOn
                map
                  (calcSlope 0.0 0.2 100.0 5.0 >>> playbackRate)
                  sl0
                map
                  (calcSlope 0.0 0.0 100.0 1.2 >>> loopStart)
                  sl1
                map
                  (calcSlope 0.0 0.05 100.0 1.0 >>> loopEnd)
                  (biSampleOn sl2 (add <$> (pure 0.0 <|> sl1)))
          ]
      D.div_
        $
          map
            ( \\{ l, f } -> D.div_
                [ text_ l
                , D.input
                    ( O.oneOfMap pure O.do
                        D.Xtype := "range"
                        D.Min := "0"
                        D.Max := "100"
                        D.Step := "1"
                        D.Value := "50"
                        D.OnInput := cb
                          ( traverse_
                              (valueAsNumber >=> f)
                              <<< (=<<) fromEventTarget
                              <<< target
                          )
                    )
                    []
                ]
            )
            [ { l: "Playback rate", f: push.slider.s0 }
            , { l: "Loop start", f: push.slider.s1 }
            , { l: "Loop end", f: push.slider.s2 }
            ] <>
            [ D.button
                ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                    start $> (music >>= push.startStop.stop)
                    event.startStop.stop <#>
                      (_ *> push.startStop.start unit)
                )
                [ text OneOf.do
                    start $> "Turn on"
                    event.startStop.stop $> "Turn off"
                ]
            ]
`,XJ=function(){return d.value}(),QJ="https://freesound.org/data/previews/100/100981_1234256-lq.mp3",rO=function(t){return function(r){return function(e){return hJ(d.value)(XJ)({wagtxt:C(`run2_
  $ loopBuf
      { buffer: buffer
      , playbackRate: 2.6
      , loopStart: 0.6
      , loopEnd: 1.1
      }
  $ OneOf.do
      bangOn
      (calcSlope 0.0 0.2 100.0 5.0 >>> playbackRate) <$> sl0
      (calcSlope 0.0 0.0 100.0 1.2 >>> loopStart) <$> sl1
      (calcSlope 0.0 0.05 100.0 1.0 >>> loopEnd) <$> biSampleOn sl2
          (add <$> (pure 0.0 <|> sl1))`),txt:C(jJ),ex1:SJ(d.value)(function(n){return function(u){var a=zy(u.startStop.start)(oD(void 0)),o=function(i){return xJ({buffer:i,playbackRate:2.6,loopStart:.6,loopEnd:1.1})(fD(CJ)(function(){return fD(Hf(function(){var f=cu(0)(.2)(100)(5);return function(m){return $J(f(m))}}())(u.slider.s0))(function(){return fD(Hf(function(){var f=cu(0)(0)(100)(1.2);return function(m){return TJ(f(m))}}())(u.slider.s1))(function(){return Hf(function(){var f=cu(0)(.05)(100)(1);return function(m){return FJ(f(m))}}())(function(f){return QM(f)(u.slider.s2)}(Hf(EJ)(zy(oD(0))(u.slider.s1))))})})}))};return Lr(MJ(OJ(function(i){return Lr([C(i.l),yi(KM(oD)(mv(wJ(di.value)("range"))(function(){return mv(PJ(Wo.value)("0"))(function(){return mv(IJ(Bo.value)("100"))(function(){return mv(RJ(Ho.value)("1"))(function(){return YM(LJ(zo.value)("50"))(function(){return NJ(qo.value)(Vr(function(){var f=UJ(BJ(Dc)(i.f)),m=WJ(yl);return function(v){return f(m(Zl(v)))}}()))})})})})})))([])])})([{l:"Playback rate",f:n.slider.s0},{l:"Loop start",f:n.slider.s1},{l:"Loop end",f:n.slider.s2}]))([on(KM(Hf(function(){var i=qJ(ee.value);return function(f){return i(Vr(w(f)))}}()))(mv(ZM(u.startStop.loading)(Vy(void 0)))(function(){return YM(tO(u.startStop.stop)(function(i){return cD(i)(cD(t(Vy(void 0)))(n.startStop.start(void 0)))}))(function(){return tO(QM(ZM(a)(HJ))(zy(oD(Vy(void 0)))(Hf(function(i){return i.value0})(e))))(function(i){return function(){i(),n.startStop.loading(void 0)();var m=Ca(Gy(zJ)(function(v){return Gy(VJ(v))(function(D){return Gy(at(v)(QJ))(function(A){return GJ(function(){var _=Y(v)([o(A)])(),k=cD(cD(_)(D))(JJ(v));return n.startStop.stop(k)(),k})})})}))();return t(function(){return n.startStop.start(void 0)(),ii(Ro(m))()})(),void 0}})})})))([Ve(fD(Hf(w("Turn off"))(u.startStop.stop))(function(){return Hf(w("Turn on"))(a)}))])]))}})})}}};var eO=q(),YJ=Yt({reflectType:function(){return`<section>
  <h2>Example 3: Fascinating rhyhtm</h2>

  <p>Ocarina comes with several different ways to hook into the Web Audio API's sample-accurate timers. In this section, we'll use a Ocarina <code>interval</code> event to create a sample-accurate ticker. We'll also use a <code>random</code> beahvior to change up our samples.</p>

  <p><code>interval :: AudioContext -> Event Number -> Event Number</code> in ocarina is similar to <a href=""><code>interval :: Int -> Event Instant</code></a> from the <code>Event</code> library with a few important exceptions.</p>

  <ul>
    <li>The ocarina interval works in seconds (<code>Number</code>) instead of milliseconds.</li>
    <li>The ocarina interval needs an audio context to work.</li>
    <li>The ocarina interval gets its timing from an <code>Event Number</code> instead of a plain old <code>Number</code>. This is necessary to have variable rates.</li>
  </ul>

  <blockquote><code>interval</code> works fine for a stream of events where each event is separated by more than ~100 milliseconds. For anything faster, you'll likely want to use <code>requestAnimationLoop</code> coupled with a local state, as it will be more efficient for older and battery-sensitive devices.</blockquote>

  <p>In the following example, we use <code>interval</code> to control the playback rate of an analogue synth. We'll also use a custom behavior called <code>random</code> to control the pitch.</p>

  <p>One important optimization we make here is the use of the function <code>memoize</code>. Whenever we're dealing with audio-ctiming, we want to limit the number of subscriptions to receive events from the audio clock. Ideally, there is only one subscription that takes a reading of the cas a single source of truth. Because we are in PureScript-land, events (like everything else), are referrentially transparent, meaning that new ones will get created every time you use them (just like a new <code>2</code> is created every time you type the value <code>2</code>: they don't all refer to one uber-<code>2</code>). To sync all the events to the <i>same</i> source, we use <code>memoize</code>. While this optimization is not necessary, I recommend it: it will make sure the timing is 100% accurate at a very small energy cost (meaning <code>memoize</code> will eat up slightly more power from a phone's battery, but still not much).</p>

  <pre><code>@txt@</code></pre>

  @ex2@

</section>
`}})({reflectType:function(){return"@"}})()()(eO(eO(N)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}})),ZJ=Ii()(De({reflectSymbol:function(){return"slider"}})()()(Mi({reflectSymbol:function(){return"startStop"}})()()()(De({reflectSymbol:function(){return"start"}})()()(De({reflectSymbol:function(){return"stop"}})()()(Yn)()())()())(Yn)()())()()),Jy=ht(qt),jy=M(X),Ba=S(ct),tj=Er(Jn),rj=Q(),ej=wf()(lm),nj=hm(_m),uj=Pt(_t),Xy=mu(_t),Qy=Du()(En),oO=Cr(),fO=dt(xr),cO={reflectSymbol:function(){return"q"}},lO={reflectSymbol:function(){return"frequency"}},pO=Sr()(),nO=Xi(ki(Zt(oO(dt(fO(ji)()()()(cO))(bi)()()()(lO)))(pO))),aj=Rl(Ok(Zt(oO(dt(fO(sT)()()()(cO))(dk)()()()(lO)))(pO))),ij=iD(Yl)(h),lD=Tc(Yl),oj=mt(Zi),fj=mt(Uf),cj=mt(Nf),lj=mt(Wf),pj=Tc(aD),_j=mt(qf),vj=mt(Bf),sj=Dn(Wt)(Me),mj=Oc(Qn),Dj=cn(Nn),bj=_u(Ht)(h),Aj=mt(ke),uO=oe(ct),kj=it(pn),Ky=W(ct),dj=K(Z),aO=M(Wt),yj=Mn(ne),gj=No(ct),Dv=rr(or),iO=mn(ne),hj=rn(Ht)(h),Sj=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import QualifiedDo.OneOfMap as O
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (text, text_)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import Effect.Random as Random
import FRP.Behavior (Behavior, behavior, sampleBy)
import FRP.Event (Event, makeEvent, memoize, subscribe)

import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import Ocarina.Clock(interval)
import Ocarina.Control (bandpass_, fan1, gain, gain_, highpass_, triangleOsc)
import Ocarina.Core (Audible, AudioEnvelope(AudioEnvelope), bangOn)
import Ocarina.Interpret (close, context)
import Ocarina.Math (calcSlope)
import Ocarina.Properties (frequency)
import Ocarina.Properties as P
import Ocarina.Run (run2e)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

type StartStop = V (start :: Unit, stop :: Effect Unit)

type UIEvents = V
  ( startStop :: StartStop
  , slider :: Number
  )

random :: Behavior Number
random = behavior \\e ->
  makeEvent \\k -> subscribe e \\f ->
    Random.random >>= k <<< f

-- pentatonic scale
cp :: Number -> Number
cp n
  | n < 0.142857 = 261.625565
  | n < 0.285714 = 293.664768
  | n < 0.428571 = 349.228231
  | n < 0.571429 = 391.995436
  | n < 0.714286 = 440.000000
  | n < 0.857143 = 523.251131
  | otherwise = 587.329536

main :: Effect Unit
main = runInBody1
  ( vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        start = event.startStop.start <|> pure unit

        music :: forall lock. _ -> Event (Array (Audible _ _))
        music evt' = memoize evt' \\evt -> do
          let
            pitch = map fst evt
            -- to avoid artifacts in the pitch change
            time = map (add 0.01 <<< snd) evt
            e0 =
              AudioEnvelope <<<
                { p: [ 0.0, 0.6, 0.2, 0.1, 0.5, 0.03, 0.0 ]
                , d: 0.4
                , o: _
                } <$> time
            e1 =
              AudioEnvelope <<<
                { p: [ 0.0, 0.3, 0.1, 0.05, 0.01, 0.005, 0.0 ]
                , d: 0.4
                , o: _
                } <$> time
            e2 =
              AudioEnvelope <<<
                { p: [ 0.0, 0.15, 0.05, 0.01, 0.005, 0.0005, 0.0 ]
                , d: 0.4
                , o: _
                } <$> time
            f0 = bangOn <|> frequency <<< cp <$> pitch
          [ fan1 (triangleOsc 0.0 f0) \\ipt _ -> do
              gain_ 2.0
                [ gain 0.0 (P.gain <$> e0)
                    [ bandpass_
                        { frequency: 1000.0
                        , q: 20.0
                        }
                        [ ipt ]
                    ]
                , gain 0.0 (P.gain <$> e1)
                    [ bandpass_
                        { frequency: 2000.0
                        , q: 20.0
                        }
                        [ ipt ]
                    ]
                , gain 0.0 (P.gain <$> e2)
                    [ highpass_
                        { frequency: 4000.0
                        , q: 20.0
                        }
                        [ ipt ]
                    ]
                ]
          ]
      D.div_
        [ D.div_
            [ text_ "tempo"
            , D.input
                ( O.oneOfMap pure O.do
                    D.Xtype := "range"
                    D.Min := "0"
                    D.Max := "100"
                    D.Step := "1"
                    D.Value := "50"
                    D.OnInput := cb
                      ( traverse_
                          (valueAsNumber >=> push.slider)
                          <<< (=<<) fromEventTarget
                          <<< target
                      )
                )
                []
            ]
        , D.button
            ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                [ start $> do
                    ctx <- context
                    let
                      myIvl = sampleBy Tuple random
                        $ interval ctx 0.91
                        $ map (calcSlope 0.0 0.42 100.0 1.4)
                        $ event.slider
                    r <- run2e ctx (music myIvl)
                    push.startStop.stop (r *> close ctx)
                , event.startStop.stop <#>
                    (_ *> push.startStop.start unit)
                ]
            )
            [ text $ oneOf
                [ start $> "Turn on"
                , event.startStop.stop $> "Turn off"
                ]
            ]
        ]
  )`,xj=Bu(function(t){return Cn(function(r){return we(t)(function(e){return function(){var u=Ui();return r(e(u))()}})})}),Cj=function(){return d.value}(),$j=function(t){if(t<.142857)return 261.625565;if(t<.285714)return 293.664768;if(t<.428571)return 349.228231;if(t<.571429)return 391.995436;if(t<.714286)return 440;if(t<.857143)return 523.251131;if(Jr)return 587.329536;throw new Error("Failed pattern match at Ocarina.Example.Docs.Events.Ex2 (line 224, column 1 - line 224, column 23): "+[t.constructor.name])},_O=function(t){return function(r){return function(e){return YJ(d.value)(Cj)({txt:C(Sj),ex2:ZJ(d.value)(function(n){return function(u){var a=Jy(u.startStop.start)(jy(void 0)),o=function(i){return ti(i)(function(f){var m=Ba(function(){var k=tj(.01);return function(G){return k(Ne(G))}}())(f),v=Ba(pu)(f),D=Jy(rj)(Ba(function(k){return ej($j(k))})(v)),A=Ba(function(k){return fm(function(G){return{p:[0,.15,.05,.01,.005,5e-4,0],d:.4,o:G}}(k))})(m),b=Ba(function(k){return fm(function(G){return{p:[0,.3,.1,.05,.01,.005,0],d:.4,o:G}}(k))})(m),_=Ba(function(k){return fm(function(G){return{p:[0,.6,.2,.1,.5,.03,0],d:.4,o:G}}(k))})(m);return[iu(nj(0)(D))(function(k){return uj(2)([Xy(0)(Ba(Qy)(_))([nO({frequency:1e3,q:20})([k])]),Xy(0)(Ba(Qy)(b))([nO({frequency:2e3,q:20})([k])]),Xy(0)(Ba(Qy)(A))([aj({frequency:4e3,q:20})([k])])])})]})};return Lr([Lr([C("tempo"),yi(ij(jy)(lD(oj(di.value)("range"))(function(){return lD(fj(Wo.value)("0"))(function(){return lD(cj(Bo.value)("100"))(function(){return lD(lj(Ho.value)("1"))(function(){return pj(_j(zo.value)("50"))(function(){return vj(qo.value)(Vr(function(){var i=sj(mj(Dc)(n.slider)),f=Dj(yl);return function(m){return i(f(Zl(m)))}}()))})})})})})))([])]),on(bj(Ba(function(){var i=Aj(ee.value);return function(f){return i(Vr(w(f)))}}()))([uO(kj(Ky(a)(dj))(Jy(jy(aO(void 0)))(Ba(function(i){return i.value0})(e))))(function(i){return function(){i();var m=yj(),v=gj(U.create)(xj)(IE(m)(.91)(Ba(cu(0)(.42)(100)(1.4))(u.slider))),D=Ul(m)(o(v))(),A=Dv(D)(iO(m));return t(Dv(A)(n.startStop.start(void 0)))(),n.startStop.stop(Dv(A)(iO(m)))()}}),uO(u.startStop.stop)(function(i){return Dv(i)(Dv(t(aO(void 0)))(n.startStop.start(void 0)))})]))([Ve(hj([Ky(a)("Turn on"),Ky(u.startStop.stop)("Turn off")]))])])}})})}}};var Fj=function(){return d.value}(),vO=function(){return Yt({reflectType:function(){return`<section>
  <h2>Three flavors of events.</h2>

  <p>When we're in the browser, events tend to come in three broad categories:</p>

  <ul>
    <li>Things that need to happen <span style="font-weight: 800;">now</span>.</li>
    <li>Things that happen as the result of a user interaction.</li>
    <li>Things that are scheduled to happen in the future, for example with <code>setTimeout</code>.</li>
  </ul>

  <p>The next three examples cover all three cases.</p>

</section>`}})({reflectType:function(){return"@"}})()()(N)(d.value)(Fj)({})}();var Mj=function(){return d.value}(),sO=function(){return Yt({reflectType:function(){return`<section>
  <h2>Events in Ocarina</h2>
  <p>Ocarina follows a consistent pattern: every audio unit accepts an event containing a <code>newtype</code> around a <code>Variant</code> of parameters that can be changed. As a motivating example, let's look at the definition of <code>sinOsc</code>.</p>

  <pre><code>sinOsc
  :: forall i outputChannels event payload
   . IsEvent event
  => Common.InitialSinOsc i
  => i
  -> event C.SinOsc
  -> C.Node outputChannels "" C.G_ event payload</code></pre>

  <p>In that defintion, <code>SinOsc</code> is a <code>newtype</code> that is defined as such:</p>

  <pre><code>newtype SinOsc = SinOsc
  (Variant (frequency :: AudioParameter, onOff :: AudioOnOff))</code></pre>

  <p>Informally, the <code>sinOsc</code> function listens to an event that contains one of several possible instructions, including changing the frequency and turning the oscillator on or off. Let's see a few more examples:</p>

    <table>
    <tr>
      <th>Audio Unit</th>
      <th>Newtype name</th>
      <th>Variant</th>
    </tr>
    <tr>
      <td><code>delay</code></td>
      <td><code>Delay</code></td>
      <td><code>Variant (delayTime :: AudioParameter)</code></td>
    </tr>
    <tr>
      <td><code>highshelf</code></td>
      <td><code>Highshelf</code></td>
      <td><code>Variant (frequency :: AudioParameter, gain :: AudioParameter)</code></td>
    </tr>
    <tr>
      <td><code>playBuf</code></td>
      <td><code>PlayBuf</code></td>
      <td><code>Variant (playbackRate :: AudioParameter, buffer :: BrowserAudioBuffer, duration :: Maybe Number, bufferOffset :: Number, onOff :: AudioOnOff)</code></td>
    </tr>
  </table>

  <p>In practice, you'll never need to use newtypes presented above. The <code>Ocarina.Properties</code> module has highly-overloaded smart constructors for all these values. For example, to use an <code>Event Number</code> to set the frequency of a <code>sinOsc</code>, you'd write <code>frequency &lt;$&gt; event</code> instead of <code>SinOsc &lt;&lt;&lt; inj (Proxy :: _ "frequency") &lt;&lt;&lt; AudioParameter &lt;&lt;&lt; { o: 0.0, t: _linearRamp, n: _ } &lt;$&gt; event</code>. Of course, you <i>can</i> write the longer format, but life is short and keystrokes are precious! Below is a table showing the varoius smart constructors available and the units to which they apply.</p>


    <table>
    <tr>
      <th>Constructor</th>
      <th>Type</th>
      <th>Receptive Audio Units</th>
    </tr>
    <tr>
      <td><code>bufferOffset</code></td>
      <td><code>Number</code></td>
      <td><ul><li><code>playBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>delayTime</code></td>
      <td><code>AudioParameter</code></td>
      <td><ul><li><code>delay</code></li></ul></td>
    </tr>
    <tr>
      <td><code>duration</code></td>
      <td><code>Maybe Number</code></td>
      <td><ul><li><code>playBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>frequency</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>allpass</code></li>
          <li><code>bandpass</code></li>
          <li><code>highpass</code></li>
          <li><code>highshelf</code></li>
          <li><code>lowpass</code></li>
          <li><code>lowshelf</code></li>
          <li><code>notch</code></li>
          <li><code>peaking</code></li>
          <li><code>periodicOsc</code></li>
          <li><code>sawtoothOsc</code></li>
          <li><code>sineOsc</code></li>
          <li><code>squareOsc</code></li>
          <li><code>triangleOsc</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>gain</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>gain</code></li>
          <li><code>highshelf</code></li>
          <li><code>lowshelf</code></li>
          <li><code>peaking</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>loopEnd</code></td>
      <td><code>Number</code></td>
      <td><ul><li><code>loopBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>loopStart</code></td>
      <td><code>Number</code></td>
      <td><ul><li><code>loopBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>offset</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>constant</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>onOff</code></td>
      <td><code>AudioOnOff</code></td>
      <td>
        <ul>
          <li><code>constant</code></li>
          <li><code>loopBuf</code></li>
          <li><code>periodicOsc</code></li>
          <li><code>playBuf</code></li>
          <li><code>sawtoothOsc</code></li>
          <li><code>sineOsc</code></li>
          <li><code>squareOsc</code></li>
          <li><code>triangleOsc</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>playbackRate</code></td>
      <td><code>AudioParameter</code></td>
      <td><ul><li><code>loopBuf</code></li><li>playBuf</li></ul></td>
    </tr>
    <tr>
      <td><code>q</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>allpass</code></li>
          <li><code>bandpass</code></li>
          <li><code>highpass</code></li>
          <li><code>lowpass</code></li>
          <li><code>notch</code></li>
          <li><code>peaking</code></li>
        </ul>
      </td>
    </tr>
  </table>

  <p>You can use this smart-constructor pattern to transform any <code>Event</code> into something that Ocarina can consume. For example:</p>

  <ul>
    <li>If you have an <code>Event Number</code> called <code>myFreq</code> and you'd like it to control the frequency of a band-pass filter, you can write <code>bandpass 440.0 (frequency &lt;$&gt; myFreq)</code>.</li>
    <li>If you have an <code>Event Number</code> called <code>myQ</code> and you'd like it to control the Q value of the same bandpass, you can write <code>bandpass 440.0 (frequency &lt;$&gt; myFreq &lt;|&gt; q &lt;$&gt; myQ)</code> <i>or</i> <code>bandpass 440.0 $ oneOf [frequency &lt;$&gt; myFreq, q &lt;$&gt; myQ]</code>.</li>
    <li>If you'd like <code>myFreq</code> <i>only</i> to have an effect when it's over <code>1000.0</code>, you can write <code>bandpass 440.0 (frequency &lt;$&gt; filter (_ > 1000.0) myFreq &lt;|&gt; q &lt;$&gt; myQ)</code>.</li>
  </ul>

  <p>None of these transformations are unique to Ocarina:</p>
  <ul>
    <li>Because <code>Event</code> implements <a href=""><code>Functor</code></a>, you can use <code>map</code> (aka <code>&lt;$&gt;</code> above).</li>
    <li>Because <code>Event</code> implements <a href=""><code>Alt</code></a>, you can use <code>alt</code> (aka <code>&lt;|&gt;</code> above).</li>
    <li>Because <code>Event</code> implements <a href=""><code>Plus</code></a>, you can use <code>empty</code> for an event that emits nothing as well as <code>oneOf</code>.</li>
    <li>Because <code>Event</code> implements <a href=""><code>Filterable</code></a>, you can use <code>filter</code>, <code>filterMap</code>, <code>partition</code>, <code>partitionMap</code>, and <code>compact</code>.</li>
    <li>Because <code>Event</code> implements <a href=""><code>IsEvent</code></a>, you can use <code>bang</code> to emit something right away, <code>sampleOn</code> to sample one event's most recent value based on another event, and all of the other functions rolled into <a href=""><code>IsEvent</code></a>.</li>
  </ul>

  <p>
    This gets to one of the core design principles of Ocarina. Idiomatic Ocarina projects use functional reactive programming as a way to "steer" web audio, and Ocarina aims to be a minimal viable framework to shepherd events to their web-audio destinations.
  </p>
</section>`}})({reflectType:function(){return"@"}})()()(N)(d.value)(Mj)({})}();var wj=function(){return d.value}(),mO=function(){return Yt({reflectType:function(){return`<section>

  <h2>Events, a primer</h2>

  <p>The <code>Event</code> and <code>Behavior</code> types in PureScript are defined as such:</p>

  <pre><code>newtype Event a =
    Event ((a -> Effect Unit) -> Effect (Effect Unit))

newtype ABehavior event a =
  ABehavior (forall b. event (a -> b) -> event b)
type Behavior = ABehavior Event
</code></pre>

  <p>Let's unpack what the contract of both types are saying.</p>

  <h3>Event</h3>

  <p>An event takes a pusher of type <code>a -> Effect Unit</code> to which you can push values of type <code>a</code>. What are the values? Whatever you want! It could be a mouse click, a slider's input, an animation loop thunk, whatever. The event returns a nested <code>Effect</code> - the outer one is triggered on event subscription and the inner one is triggered on event unsubscription. In the case of a click listener, for example, the outer effect will likely call <code>addEventListener</code> and the inner will likely call <code>removeEventListener</code>.</p>

  <p>
    When using Ocarina, you have to get your events from somewhere. At a minimum, you'll consume a browser interaction like a click or swipe that turns on the audio. In fact, without some form of human interaction, most browsers will bthe Web Audio API from turning on.
  </p>
  <p>
    <code>Events</code> are often produced within a web framework like <a href="https://github.com/mikesol/purescript-deku">Deku</a>, Halogen or React. They don't have to be, though - you can create and consume your own events.
  </p>

  <h3>Behavior</h3>

  <p>
    The <code>Behavior</code> type takes an event that needs to be "unlocked" (meaning in the form of <code>a -> b</code>, so an <code>a</code> is needed to una <code>b</code>) and unlocks it with an <code>a</code>. Behaviors don't need to produce their <code>a</code> immediately. In fact, they don't need to produce it at all: it's entirely possible to create <code>Behavior (const empty)</code> that "mutes" the event. This resembles the physical world: when we want to observe a behavior, like the weather outside or the axial rotation of the Earth, there is a time-cost to observing anything that ranges from instantaneous to infinite.
  </p>

  <p>
    In Ocarina, we usually want to observe the behavior of things like a mouse's position, an audio buffer's content or a random number generator.
  </p>
</section>`}})({reflectType:function(){return"@"}})()()(N)(d.value)(wj)({})}();var Ij=ou(qt)(X),Rj=rr(or),tp=q(),Lj=Yt({reflectType:function(){return`<div>
  <h1>Events</h1>

  <h3>Clicks, wiggles and loops, oh my!</h3>
  <p>
    The true magic of web audio lies in its ability to harness the rich interactivity built into the browser. We can use mouse clicks, finger swipes and animation loops to create beautiful audio landscapes. But how can we tame the complexity of all these events in an expressive, declarative, functional manner? Enter <code>Event</code>, the abstraction that allows us to build rich reactive works using Ocarina.
  </p>

  @primer@
  @inOcarina@
  @flavors@
  @ex0@
  @ex1@
  @ex2@

  <h2>Next steps</h2>
  <p>In this section, saw how to build rich audio applications using the <code>Event</code> and <code>Behavior</code> types. We also covered the three most common patterns you'll see when working with events: events that need to happen <i>now</i>, events that come from user interaction, and timed events. In the next section, we'll look at different ways to specify <a @next@ style="cursor:pointer;">the numeric parameters being sent as events</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(tp(tp(tp(tp(tp(Je()(tp(N)({reflectType:function(){return"primer"}})({reflectSymbol:function(){return"primer"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"inOcarina"}})({reflectSymbol:function(){return"inOcarina"}}))({reflectType:function(){return"flavors"}})({reflectSymbol:function(){return"flavors"}}))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}}));var Nj=function(){return d.value}(),DO=function(t){return function(r){return function(e){return function(n){var u=function(o){return Ij(n)(Rj(r(o))(Xe))},a=fu(t)(e);return Lj(d.value)(Nj)({next:u(q_.value),primer:mO,inOcarina:sO,flavors:vO,ex0:VM(a)(r)(n),ex1:rO(a)(r)(n),ex2:_O(a)(r)(n)})}}}};var Bj=Yt({reflectType:function(){return`<div>
  <pre><code>\\{ tink0, tink1, tink2, tink3 } -> run2_
  [ gain_ 1.0 do
      let ooo n = pure $ onOff $ dt (add n) apOn
      [ playBuf tink0 (ooo 0.1)
      , playBuf tink1 (ooo 0.2)
      , playBuf tink2 (ooo 0.9)
      , playBuf tink3 (ooo 1.8)
      ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(q()(N)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),Wj=ai(Ku),Yy=it(D_),qj=S(ml),pD=Qu(Ku),Hj=Pt(_t),zj=M(X),Vj=Cc()(kc),Gj=cm(),Jj=Er(Jn),_D=au(Ia),jj=function(){return d.value}(),bO=function(t){return function(r){return function(e){return Bj(d.value)(jj)({ai0:vt(e)(t)(function(n){return Wj(Yy(Yy(Yy(qj(function(u){return function(a){return function(o){return function(i){return{tink0:u,tink1:a,tink2:o,tink3:i}}}}})(pD(at(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(pD(at(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(pD(at(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(pD(at(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(u){return Y(n)([Hj(1)(function(){var a=function(o){return zj(Vj(Gj(Jj(o))(E_)))};return[_D(u.tink0)(a(.1)),_D(u.tink1)(a(.2)),_D(u.tink2)(a(.9)),_D(u.tink3)(a(1.8))]}())])}})})}}};var Qj=Yt({reflectType:function(){return`<div>
  <pre><code>\\{ tink0, tink1, tink2, tink3 } -> run2_
  [ gain_ 1.0
      $ do
          let
            ooo n = pure $ onOff $ dt (add n) apOn
            mtk i =
              case i \`mod\` 4 of
                0 -> tink0
                1 -> tink1
                2 -> tink2
                _ -> tink3
          0 .. 100 &lt;#&gt;
            \\i' -> do
              let i = toNumber i'
              playBuf (mtk i')
                (ooo (0.3 + 0.3 * (i * (1.005 \`pow\` i))))</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(q()(N)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),Kj=ai(Ku),Zy=it(D_),Yj=S(ml),vD=Qu(Ku),Zj=Pt(_t),t7=M(X),r7=Cc()(kc),e7=cm(),n7=Er(Jn),u7=hu(Ga),a7=oe(ir),i7=au(Ia),o7=function(){return d.value}(),AO=function(t){return function(r){return function(e){return Qj(d.value)(o7)({ai0:vt(e)(t)(function(n){return Kj(Zy(Zy(Zy(Yj(function(u){return function(a){return function(o){return function(i){return{tink0:u,tink1:a,tink2:o,tink3:i}}}}})(vD(at(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(vD(at(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(vD(at(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(vD(at(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(u){return Y(n)([Zj(1)(function(){var a=function(i){return t7(r7(e7(n7(i))(E_)))},o=function(i){var f=u7(i)(4);return f===0?u.tink0:f===1?u.tink1:f===2?u.tink2:u.tink3};return a7(Ke(0)(100))(function(i){var f=Br(i);return i7(o(i))(a(.3+.3*(f*vf(1.005)(f))))})}())])}})})}}};var c7=Yt({reflectType:function(){return`<div>
  <pre><code>run2_
  [ fan1 (loopBuf buf bangOn)
      \\b _ -> gain_ 0.8
        [ bandpass_ { frequency: 400.0, q: 1.0 } [ b ]
        , bandpass_ { frequency: 880.0, q: 5.0 } [ b ]
        , bandpass_ { frequency: 1200.0, q: 10.0 } [ b ]
        , bandpass_ { frequency: 2000.0, q: 20.0 } [ b ]
        , bandpass_ { frequency: 3000.0, q: 30.0 } [ b ]
        ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(q()(N)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),l7=tr(cr),p7=Q(),_7=Pt(_t),bv=Xi(ki(Zt(Cr()(dt(dt(xr)(ji)()()()({reflectSymbol:function(){return"q"}}))(bi)()()()({reflectSymbol:function(){return"frequency"}})))(Sr()()))),v7=function(){return d.value}(),kO=function(t){return function(r){return function(e){return c7(d.value)(v7)({ai0:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([iu(l7(u)(p7))(function(a){return _7(.8)([bv({frequency:400,q:1})([a]),bv({frequency:880,q:5})([a]),bv({frequency:1200,q:10})([a]),bv({frequency:2e3,q:20})([a]),bv({frequency:3e3,q:30})([a])])})])}})})}}};var m7=Yt({reflectType:function(){return`<div>
  <pre><code>\\buf -> run2_
  [ fan1 (loopBuf buf bangOn)
      \\b _ -> gain_ 0.8
        $ 0 .. 40 &lt;#&gt; lcmap toNumber
            \\i -> bandpass_
              { frequency: 200.0 + i * 150.0, q: 30.0 }
              [ b ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(q()(N)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),D7=tr(cr),b7=Q(),A7=Pt(_t),k7=oe(ir),d7=Ju(Wn),y7=Xi(ki(Zt(Cr()(dt(dt(xr)(ji)()()()({reflectSymbol:function(){return"q"}}))(bi)()()()({reflectSymbol:function(){return"frequency"}})))(Sr()()))),g7=function(){return d.value}(),dO=function(t){return function(r){return function(e){return m7(d.value)(g7)({ai0:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([iu(D7(u)(b7))(function(a){return A7(.8)(k7(Ke(0)(40))(d7(Br)(function(o){return y7({frequency:200+o*150,q:30})([a])})))})])}})})}}};var S7=Yt({reflectType:function(){return`<div>
  <pre><code>\\buf -> run2_
  [ fix
      \\b -> gain_ 1.0
        [ playBuf buf bangOn
        , delay_ 0.1 [ gain_ 0.6 [ b ] ]
        ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(q()(N)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),yO=Pt(_t),x7=au(Ia),C7=Q(),$7=hf(Ai),T7=function(){return d.value}(),gO=function(t){return function(r){return function(e){return S7(d.value)(T7)({ai0:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(u){return Y(n)([wu(function(a){return yO(1)([x7(u)(C7),$7(.1)([yO(.6)([a])])])})])}})})}}};var hO=q(),E7=Yt({reflectType:function(){return`<div>
  <pre><code>@txt@</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(hO(hO(N)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),M7=au(Ia),O7=Q(),SO=Pt(_t),xO=mu(_t),w7=function(){return d.value}(),P7=function(t){var r=M(t);return function(e){return r(Du(e)(En)({p:[1,1,0],o:0,d:10}))}},I7=P7(X)(),R7=function(t){var r=M(t);return function(e){return r(Du(e)(En)({p:[1,1,0],o:0,d:8}))}},L7=R7(X)(),N7=function(t){var r=hf(t);return function(e){var n=Pt(e);return function(u){var a=Rl(u);return function(o){return function(i){return function(f){return function(m){return r(o)([n(i)([a(f)(m)])])}}}}}}},rp=N7(Ai)(_t)(Pl),CO=function(t){return function(r){return function(e){return E7(d.value)(w7)({txt:C(`dgh d g h i =
  delay_ d [gain_ g [highpass_ h i]]

fade0 = pure
  $ P.gain
  $ AudioEnvelope { p: [1.0, 1.0, 0.0], o: 0.0, d: 8.0 }

fade1 = pure
  $ P.gain
  $ AudioEnvelope { p: [1.0, 1.0, 0.0], o: 0.0, d: 10.0 }

scene buf = run2_
  [ fan1 (playBuf buf bangOn) \\b _ -> mix $ fix
      \\g0 -> gain_ 1.0
        [ b
        , dgh 0.15 0.7 1500.0
            [ fix
                \\g1 -> gain 1.0 fade1
                  [ dgh 0.4 0.5 2500.0
                      [ g0, g1 ]
                  ]
            ]
        , dgh 0.29 0.85 2000.0
            [ fix
                \\g1 -> gain_ 1.0
                  [ dgh 0.6 0.6 3500.0
                      [ g0
                      , ( fix
                            \\g2 -> gain 1.0 fade0
                              [ dgh 0.75 0.6 4000.0
                                  [ g1, g2 ]
                              , dgh 0.75 0.55 3000.0 [ b ]
                              ]
                        )
                      ]
                  ]
            ]
        ]
  ]`),ai0:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(u){return Y(n)([iu(M7(u)(O7))(function(a){return wu(function(o){return SO(1)([a,rp(.15)(.7)(1500)([wu(function(i){return xO(1)(I7)([rp(.4)(.5)(2500)([o,i])])})]),rp(.29)(.85)(2e3)([wu(function(i){return SO(1)([rp(.6)(.6)(3500)([o,wu(function(f){return xO(1)(L7)([rp(.75)(.6)(4e3)([i,f]),rp(.75)(.55)(3e3)([a])])})])])})])])})})])}})})}}};var B7=ou(qt)(X),W7=rr(or),q7=Yt({reflectType:function(){return`<section>
  <p>
    In the <a @hwLink@ style="cursor:pointer;">hello world</a> section, we saw how to create and wire up two audio nodes: a <code>sinOsc</code>, or a sine-wave oscillator, is hooked up to a <code>gain</code> node. For some cases, feeding one audio node to another all the way up to a loudspeaker will be all you need. However, in most cases, you'll need to exploit three additional relationships:</p>
    <ul>
      <li><span style="font-weight:800px;">Many to one</span>, where many audio units pass through one.</li>
      <li><span style="font-weight:800px;">One to many</span>, where a single audio unit passes through many different ones.</li>
      <li><span style="font-weight:800px;">Feedback</span>, where an audio unit is an input to itself.</li>
    </ul>
    <p>This section will show how ocarina handles all three cases:</p>
    <ul>
      <li><span style="font-weight:800px;"><code>Array</code></span> is the data structure we'll use to send many audio units into one.</li>
      <li><span style="font-weight:800px;"><code>fan</code></span> is a function that we'll use to "fan" one audio node out to many.</li>
      <li><span style="font-weight:800px;"><code>fix</code></span> is the function we'll use to make an audio unit an input into itself.</li>
    </ul>
    <h2>The setup</h2>
    <p>
      To illustrate how <code>Array</code>, <code>fan</code> and <code>fix</code> work, we're going to use a few new audio units.
    </p>
      <ul>
        <li><code>delay</code>: A delay node</li>
        <li><code>bandpass</code>: A bandpass filter, meaning a filter that lets a single frequency band pass through.</li>
        <li><code>playBuf</code> and <code>loopBuf</code>: Playback or looping playback of a buffer. We'll use some MP3 files from freesound.org.</li>
      </ul>
  </section>`}})({reflectType:function(){return"@"}})()()(Je()(N)({reflectType:function(){return"hwLink"}})({reflectSymbol:function(){return"hwLink"}}));var H7=function(){return d.value}(),$O=function(t){return function(r){return function(e){return function(n){var u=function(a){return B7(n)(W7(r(a))(Xe))};return q7(d.value)(H7)({hwLink:u(hc.value)})}}}};var V7=ou(qt)(X),G7=rr(or),Fc=q(),J7=Yt({reflectType:function(){return`<div>
  <h1>Array, fan, and fix</h1>

  <h3>The anatomy of a Ocarina graph</h3>

  @intro@

  <h2>Arrays</h2>

  <p>To send several audio units through one, we use an <code>Array</code>.</p>

  @code0@

  <p>PureScript <code>Array</code>-s are extremely flexible and efficient, so go to town! For example, you can <code>map</code> (aka <code>&lt;#&gt;</code> when flipped) over a range of integers to create audio units, like in the example below.</p>

  @code1@

  <h2>Fan</h2>

  <p><span style="font-weight:800;">Fan</span> takes a vector of audio signals and fans it out to multiple processing chains. If you have a single signal, you can use <code>fan1</code>. For example, if you have a looping buffer and you'd like to filter it through a bank of different filters, you can do this via fan. Fan takes two arguments:</p>

  <ul>
    <li>A vector of nodes to fan out (or a single node in the case of <code>fan1</code>).</li>
    <li>A function that accepts a reference to this/these node(s) and returns a new node that may or may not contain the input.</li>
  </ul>

  <p>Let's see an example below that fans one <code>playBuf</code> to five bandpass filters.</p>

  @code2@

  <p>Just for kicks, let's jack it up to forty bandpass filters.</p>

  @code3@

  <h2>Fix</h2>

  <p><span style="font-weight:800;">Fix</span> is a fixed point operator. It accepts itself as an argument and returns... itself \u{1F92F}. You can use <code>fix</code> to create feedback loops!</p>

  @code4@

  <blockquote>If you don't have some sort of delay line in your processing chain, either via the Web-Audio-provided delay line or a custom delay node, Web Audio will raise a runtime error. Ocarina doesn't check for this, so make sure you test your audio to guarantee that it's feedback-explosion-free!</blockquote>

  <p>Nothing stops you from nesting <code>fix</code>-s to create a mega-feedback loop!</p>

  <blockquote>In the example below, I've added a couple fades to make sure the experience isn't too unpleasant. We'll talk more about fades in the events section \u{1F3B8}</blockquote>

  @code5@

  <h2>Next steps</h2>
  <p>In this section, saw how to combine together audio nodes with arrays, fan one audio node to many processing chains via <code>fan</code>, and how to create a fixed point, aka feedback, for a node via <code>fix</code>. In the next section, we'll ramp up on all of the yummy <a @next@ style="cursor:pointer;">audio nodes you can use</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(Fc(Fc(Fc(Fc(Fc(Fc(Fc(Je()(N)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"intro"}})({reflectSymbol:function(){return"intro"}}))({reflectType:function(){return"code5"}})({reflectSymbol:function(){return"code5"}}))({reflectType:function(){return"code4"}})({reflectSymbol:function(){return"code4"}}))({reflectType:function(){return"code3"}})({reflectSymbol:function(){return"code3"}}))({reflectType:function(){return"code2"}})({reflectSymbol:function(){return"code2"}}))({reflectType:function(){return"code1"}})({reflectSymbol:function(){return"code1"}}))({reflectType:function(){return"code0"}})({reflectSymbol:function(){return"code0"}}));var j7=function(){return d.value}(),TO=function(t){return function(r){return function(e){return function(n){var u=function(o){return V7(n)(G7(r(o))(Xe))},a=fu(t)(e);return J7(d.value)(j7)({intro:$O(t)(r)(e)(n),next:u(B_.value),code0:bO(a)(r)(n),code1:AO(a)(r)(n),code2:kO(a)(r)(n),code3:dO(a)(r)(n),code4:gO(a)(r)(n),code5:CO(a)(r)(n)})}}}};var FO=O("code"),tg=FO(x(h));var EO=O("pre"),rg=EO(x(h));var Y7=rr(or),MO=q(),Z7=Yt({reflectType:function(){return`<div>
  <h1>Hello world</h1>

  <h3>Wagging at 440Hz</h3>

  <p>Here's a "hello world" in Ocarina. In this and all the following sections, we'll start with a full example, and we'll pick it apart afterwards.</p>

  @code@
  @result@

  <h2>The <code>run</code> functions</h2>

  <p>The <code>run</code> family of functions run our audio and produces an unsubscribe function that we use to stop the audio. In this case, <code>run2_</code> does three extra things:
  <ul>
    <li>Wires up our session for two-channel audio. If the sources are mono, it will automatically scale them up to stereo.</li>
    <li>Automatically handles creation and destruction of audio contexts.</li>
    <li>Takes care of the subscription to the rendering engine.</li>
  </ul></p>

  <p>The <code>push</code> function comes from the <a href="https://github.com/mikesol/purescript-deku"><code>purescript-deku</code></a> framework and is used to register an unsubscribe effect. When we trigger the effect it in the <code>Just</code> branch of our pattern match, the audio turns off.</p>

  <h2>Audio units</h2>

  <p>The sound you hear when you play the example above is created with the statement <code>gain_ 0.15 [ sinOsc 440.0 bangOn ]</code>. The first function, <code>gain_</code>, creates a gain node with a volume of <code>0.15</code>. In WebAudio, gain ranges from <code>0.0</code> to <code>1.0</code> and can be converted to decibels using the following equation:</p>

  <pre><code>decibels = 20 * log10( gain );</code></pre>

  <p>In our case, a gain of <code>0.15</code> is roughly <code>-16.5 dB</code>.</p>

  <p>Our sine wave oscillator is set to a frequency of <code>440Hz</code>. That means that your loudspeaker or headphones will vibrate back and forth in sinusoidal motion 440 times per second, which most folks perceive as the <a href="https://en.wikipedia.org/wiki/A440_(pitch_standard)">note A</a>. And we turn on the oscillator with <code>bangOn</code>, as the default is off for <i>all</i> sound generators in Ocarina. This is a design decision to help preserve the hearing of those that work frequently with audio.</p>

  <h2>Next steps</h2>
  <p>Now that we have our setup running, let's explore the anatomy of a Ocarina graph. Irrespective of the nodes comprising the graph, there are three basic concepts you need to be familiar with before you start diving into audio units: <a @next@ style="cursor:pointer;">array, fan, and fix</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(MO(Je()(MO(N)({reflectType:function(){return"result"}})({reflectSymbol:function(){return"result"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}})),tX=M(ru),rX=Pt(_t),eX=gc(gf),nX=Q(),uX=ou(qt)(X),aX=function(){return d.value}(),OO=function(t){return function(r){return function(e){return function(n){var u=Y7(r(W_.value))(Xe),a=fu(t)(e);return Z7(d.value)(aX)({code:rg([tg([C(`case e of
  Just x -> x *> push Nothing
  _ -> (run2_ [ gain_ 0.15 [ sinOsc 440.0 bangOn ] ]
         >>= Just >>> push`)])]),result:vt(n)(a)(function(o){return tX(void 0)})(function(o){return function(i){return Y(o)([rX(.15)([eX(440)(nX)])])}}),next:uX(n)(u)})}}}};var wO=ul;var PO=function(){return function(t){return t}};var IO=function(){return function(t){return t}};var eg=function(){function t(){}return t.value=new t,t}();var RO={attr:function(t){return function(r){return c({key:"height",value:s(r)})}}};var ng=function(){function t(){}return t.value=new t,t}();var LO={attr:function(t){return function(r){return c({key:"width",value:s(r)})}}};var ug=O("canvas");var ag=function(){function t(){}return t.value=new t,t}();var NO={attr:function(t){return function(r){return c({key:"@self@",value:J(r)})}}};function sD(t){return function(){return t.getContext("2d")}}function Av(t){return function(r){return function(){t.fillStyle=r}}}function mD(t){return function(){t.beginPath()}}function DD(t){return function(){t.fill()}}function ig(t){return function(r){return function(){t.arc(r.x,r.y,r.radius,r.start,r.end,r.useCounterClockwise)}}}function bD(t){return function(r){return function(){t.fillRect(r.x,r.y,r.width,r.height)}}}var GO=Et(gu),hX=Yt({reflectType:function(){return"<section>@ex1@</section>"}})({reflectType:function(){return"@"}})()()(q()(N)({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})),SX=Ii()(De({reflectSymbol:function(){return"canvas"}})()()(De({reflectSymbol:function(){return"slider"}})()()(Mi({reflectSymbol:function(){return"startStop"}})()()()(De({reflectSymbol:function(){return"loading"}})()()(De({reflectSymbol:function(){return"start"}})()()(De({reflectSymbol:function(){return"stop"}})()()(Yn)()())()())()())(Yn)()())()())()()),AD=ht(qt),kv=M(X),aa=S(ct),JO=Cr(),jO=dt(xr),XO=Sr()(),xX=gm(ym(Zt(JO(dt(jO(dm)()()()({reflectSymbol:function(){return"fftSize"}}))(km)()()()({reflectSymbol:function(){return"cb"}})))(XO))),og=M(Wt),CX=M(Ha),$X=au(Ia),TX=Q(),FX=Uu()(Pa),ep=xl(Af),UO=Pt(_t),EX=N_(wk(Zt(JO(dt(jO(mT)()()()({reflectSymbol:function(){return"maxDelayTime"}}))(yk)()()()({reflectSymbol:function(){return"delayTime"}})))(XO))),kD=ly()(Pa),fg=mu(_t),MX=Du()(Pa),zf=x(h),cg=_u(Ht)(h),OX=mt(LO),wX=mt(RO),PX=mt(Bx),BO=mt(NO),lg=Dn(Wt)(Me),IX=mt(Zi),RX=mt(Uf),LX=mt(Nf),NX=mt(Wf),UX=mt(qf),BX=mt(Ux),WX=mt(Bf),qX=Oc(Qn),HX=cn(Nn),WO=rn(Ht)(h),zX=mt(ol),VX=mt(ke),qO=W(ct),HO=oe(ct),dv=rr(or),GX=it(pn),JX=K(Z),dD=lt(qn),jX=Mn(Ge),XX=Na(Ge),QX=S(To),KX=IO(),YX=uC(Ku)(wO),ZX=PO(),zO=de(Ge),t9=Qb(),r9=en(xi)(Wt),e9=$u(Wt)(Me),n9=S(L),u9=S(ir),a9=Im(Pm),i9=mn(ne),o9=function(){return 2*sf}(),np=function(t){return{o:t.value0+.04,n:t.value1,t:Ji}};var f9=function(){return d.value}(),c9=function(t){var r=M(t);return function(e){var n=wf(e)(En);return function(u){return function(a){return r(n({p:[u,a],o:0,d:16}))}}}},up=c9(X)(),l9=function(t){var r=M(t);return function(e){return r(Du(e)(En)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:18}))}},p9=l9(X)(),_9=function(t){var r=M(t);return function(e){return r(Du(e)(En)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:24}))}},v9=_9(X)();var s9=function(t){var r=N_(t);return function(e){var n=mu(e);return function(u){var a=Nk(u);return function(o){return function(i){return function(f){return function(m){return function(v){return function(D){return function(A){return r(o)(i)([n(f)(m)([a(v)(D)(A)])])}}}}}}}}}},yD=s9(Ai)(_t)(Pl),m9=function(t){var r=N_(t);return function(e){var n=mu(e);return function(u){var a=Lk(u);return function(o){return function(i){return function(f){return function(m){return function(v){return function(D){return function(A){return r(o)(i)([n(f)(m)([a(v)(D)(A)])])}}}}}}}}}},VO=m9(Ai)(_t)(BT),D9=function(t){var r=M(t);return function(e){var n=ly(e)(En);return function(u){return function(a){return r(n({p:[u,a],o:0,d:16}))}}}},b9=D9(X)(),QO=400,pg=Br(QO),A9=function(){return GO(QO)+"px"}(),KO=600,_g=Br(KO),k9=function(){return GO(KO)+"px"}(),d9={pluck0:"https://freesound.org/data/previews/493/493016_10350281-lq.mp3",pluck1:"https://freesound.org/data/previews/141/141524_2558140-lq.mp3",strum0:"https://freesound.org/data/previews/234/234738_3635427-lq.mp3"},YO=function(t){return function(r){return function(e){return hX(d.value)(f9)({ex1:SX(d.value)(function(n){return function(u){var a=AD(kv(void 0))(u.startStop.start),o=function(i){return function(f){return function(m){var v=aa(function(D){return new U(D.acTime,D.value)})(Gl(i)(u.slider));return[xX({cb:function(D){return function(){return te(new F(D))(m)(),te(P.value)(m)}},fftSize:im.value})(CX(iu($X(f)(AD(TX)(aa(function(){var D=ep(cu(0)(.96)(100)(1.04));return function(A){return FX(np(D(A)))}}())(v))))(function(D){return wu(function(A){return UO(1)([D,EX({maxDelayTime:2.5,delayTime:1})(aa(function(){var b=ep(cu(0)(.5)(100)(2.45));return function(_){return kD(np(b(_)))}}())(v))([fg(.4)(aa(function(){var b=ep(cu(0)(.6)(100)(.9));return function(_){return MX(np(b(_)))}}())(v))([D])]),yD(.15)(zf)(.7)(zf)(1500)(up(1500)(3e3))([wu(function(b){return fg(1)(p9)([yD(.4)(zf)(.5)(zf)(3e3)(up(3e3)(100))([A,b])])})]),yD(.29)(aa(function(){var b=ep(cu(0)(.1)(100)(.4));return function(_){return kD(np(b(_)))}}())(v))(.85)(zf)(2e3)(up(2e3)(5e3))([wu(function(b){return UO(1)([yD(.6)(aa(function(){var _=ep(cu(0)(.8)(100)(.3));return function(k){return kD(np(_(k)))}}())(v))(.6)(zf)(3500)(up(3500)(100))([A,wu(function(_){return fg(1)(v9)([VO(.75)(aa(function(){var k=ep(cu(0)(.9)(100)(.1));return function(G){return kD(np(k(G)))}}())(v))(.6)(zf)(4e3)(up(4e3)(200))([b,_]),VO(.75)(b9(.75)(.2))(.55)(zf)(200)(up(200)(4e3))([D])])})])])})])])})})))]}}};return Lr([ug(AD(cg(kv)([OX(ng.value)(k9),wX(eg.value)(A9),PX(Ot.value)("width: 100%;"),BO(ag.value)(function(){var i=lg(function(f){return function(){var v=sD(f)();return Av(v)("black")(),bD(v)({width:_g,height:pg,x:0,y:0})(),void 0}});return function(f){return i(HA(f))}}())]))(aa(function(i){return BO(ag.value)(function(){var f=lg(function(m){return function(){var D=sD(m)();return Av(D)("black")(),bD(D)({width:_g,height:pg,x:0,y:0})(),Av(D)("rgba(255,255,255,0.2)")(),bp(i)(function(A){return function(){return mD(D)(),ig(D)({end:o9,radius:A.value1*40,start:0,x:A.value0.x*_g,y:A.value0.y*pg,useCounterClockwise:!1})(),DD(D)()}})()}});return function(m){return f(HA(m))}}())})(u.canvas)))([]),yi(cg(kv)([IX(di.value)("range"),RX(Wo.value)("0"),LX(Bo.value)("100"),NX(Ho.value)("1"),UX(zo.value)("50"),BX(Ot.value)("width: 100%;"),WX(qo.value)(Vr(function(){var i=lg(qX(Dc)(n.slider)),f=HX(yl);return function(m){return i(f(Zl(m)))}}()))]))([]),on(WO([kv(zX(Ot.value)("width:100%; padding:1.0rem;")),cg(aa(function(){var i=VX(ee.value);return function(f){return i(Vr(w(f)))}}()))([qO(u.startStop.loading)(og(void 0)),HO(u.startStop.stop)(function(i){return dv(i)(dv(t(og(void 0)))(n.startStop.start(void 0)))}),HO(GX(qO(a)(JX))(AD(kv(og(void 0)))(aa(function(i){return i.value0})(e))))(function(i){return function(){i(),n.startStop.loading(void 0)();var m=Or(P.value)(),v=Ca(dD(jX)(function(D){return dD(XX(D))(function(A){return dD(QX(KX)(YX(at(D))(ZX(d9))))(function(b){return dD(zO(y_(0)(5e4)))(function(_){var k=bf(WA(Sn(t_(b.pluck0))(nl(Xb(t9(b))))))({newSeed:Df(_),size:4});return zO(function(){var nt=r9(function(z){return function(){var st=Ui(),qr=Ui();return{x:st,y:qr}}})(Ke(0)(127))(),bt=Y(D)(o(D)(k)(m))(),vr=we(xf)(function(z){return function(){var st=Ur(m)();return e9(st)(function(qr){return function(){var fn=V_(qr)(),On=n9(function(){var ia=Rp(nt),be=u9(function(ue){return function(Wu){return Wu/255}(ue)});return function(ue){return ia(be(ue))}}())(a9(fn))();return n.canvas(On)(),void 0}})()}})(),Gr=dv(dv(dv(bt)(A))(i9(D)))(vr);return n.startStop.stop(Gr)(),Gr})})})})}))();return t(function(){return n.startStop.start(void 0)(),ii(Ro(v))()})(),void 0}})])]))([Ve(WO([aa(w("Turn off"))(u.startStop.stop),aa(w("Turn on"))(a),aa(w("Loading..."))(u.startStop.loading)]))])])}})})}}};var g9=Dt({reflectType:function(){return`<div>
  <h1>Ocarina</h1>

  <h3>A web-audio framework written in PureScript</h3>

  <p>Hi! You've found <a href="https://github.com/mikesol/purescript-ocarina">Ocarina</a>.</p>

  <p>Ocarina is a web-audio framework designed for interactive media and games. Events like mouse clicks, MIDI notes and tweening frames are streamed to an audio rendering engine and, in response to these events, sound happens.</p>

  <h2>Why?</h2>

  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API">Web Audio API</a> is an amazing piece of technology. It is clear, concise, straightforward and ergonomic. So why build a framework on top of it?</p>

  <p>As audio projects become more and more ambitious, a need emerges for powerful abstractions to handle browser events and scheduling. Ocarina tackles this problem through a small set of <a href="https://en.wikipedia.org/wiki/Functional_reactive_programming">FRP</a>-based abstractions. In doing so, it aims to be concise, expressive, and as fast as manually-optimized hand-written JavaScript.</p>

  <h2>How does it sound?</h2>

  <p>Here's a small example in Ocarina that, when you turn it on, emits a single sound and then uses feedback loops to create long tail. You can use the slider to change the properties of the tail in real time.</p>

  ~ex~

  <p>By the end of this documentation, you'll know all of the concepts you need to create interactive audio like the example above.</p>

  <p>If you'd like to use this documentation as a springboard for your own work, it can be found <a href="https://github.com/mikesol/purescript-ocarina/tree/main/examples/docs">here</a>.</p>

  <p>And now, without further ado, let's write a small <a ~next~ style="cursor:pointer;">hello world \xE0 la ocarina</a>!</p>
</div>`}})()()(q()(Je()(N)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"ex"}})({reflectSymbol:function(){return"ex"}})),h9=ou(qt)(X),S9=rr(or),x9=function(){return d.value}(),ZO=function(t){return function(r){return function(e){return function(n){var u=fu(t)(e);return g9(x9)({next:h9(n)(S9(r(hc.value))(Xe)),ex:YO(u)(r)(n)})}}}};var $9=Dt({reflectType:function(){return`<div>
  <h1>Merge and split</h1>

  <h3>Inputs and outputs abound!</h3>
  <p>
    Web audio allows you to merge and split arbitrary audio. This is essential when you're working with complex audio setups like 5.1 surround sound or novel headphones used in some gaming setups. Ocarina allows you to both split and merge arbitrary signals using Web Audio's native merger and splitter nodes.
  </p>

  <h2>Merging</h2>

  <p>Merging audio in ocarina looks like any node that takes multiple inputs, but instead of accepting something of type <code>AudioInput</code>, it accepts a <i>vector of audio inputs</i>.</p>

  <h2>Splitting</h2>

  <p>Splitting is the inverse operation of merging: it takes a single audio node and splits it into its separate channels. In doing so, it resembles <code>fan</code>, but instead of fanning the audio, it splits it into mono-channel audio.</p>

  <h2>Next steps</h2>
  <p>In this section, saw how to merge and split audio. In the next section, we'll look at how to work with <a ~next~ style="cursor:pointer;">custom audio worklets</a>.</p>
</div>`}})()()(Je()(N)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})),T9=M(X),F9=mt(ke),E9=rr(or),M9=function(){return d.value}(),tw=function(t){return function(r){return function(e){return function(n){return $9(M9)({next:T9(F9(ee.value)(Vr(w(E9(r(xm.value))(Xe)))))})}}}};var rw=q(),w9=Dt({reflectType:function(){return`<section>
  <h2>Cancel</h2>
  <p>The <code>AudioCancel</code> parameter corresponds to the Web Audio API's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/cancelScheduledValues"><code>cancelScheduledValues</code></a> function and cancels whatever effects you programmed in the future. In the example below, we execute the following sequence:</p>
  <ol>
    <li>Play an audio file</li>
    <li>Send an event at 1.0 seconds to schedule an evenlope to modulate the audio rate starting at 1.5 seconds.</li>
    <li>Cancel the envelope at 3.0 seconds, but schedule the cancelation to take effect at 4.0 seconds.</li>
  </ol>
  <pre><code>~txt~</code></pre>
  ~cancel~
  </section>
`}})()()(rw(rw(N)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}})),P9=Pt(_t),I9=tr(cr),R9=rn(Ht)(h),L9=Q(),ew=M(X),nw=Uu(),N9=nw(En),U9=qu(hi),B9=W(ir),W9=nw(J0),q9=function(){return d.value}(),uw=function(t){return function(r){return function(e){return w9(q9)({txt:C(`\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf OneOf.do
          bangOn
          delay 1000
            $ pure
            $ playbackRate
            $ AudioEnvelope
                { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                , o: 1.5
                , d: 30.0
                }
          delay 3000 (pure (playbackRate (AudioCancel { o: 3.5 })))
      ]
  ]`),cancel:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([P9(1)([I9(u)(R9([L9,ga(1e3)(ew(N9({p:U9(B9(Ke(0)(60))([1,1.2,1,.8])),o:1.5,d:30}))),ga(3e3)(ew(W9({o:3.5})))]))])])}})})}}};var aw=q(),z9=Dt({reflectType:function(){return`<section>
  <h2>Envelope</h2>
  <p>The <code>AudioEnvelope</code> parameter corresponds to the Web Audio API's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setValueCurveAtTime"><code>setValueCurveAtTime</code></a> function and sets an envelope <code>p</code> over the duration <code>d</code> starting at time <code>o</code>.</p>
  <pre><code>~txt~</code></pre>
  ~envelope~
  </section>
`}})()()(aw(aw(N)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}})),V9=Pt(_t),G9=tr(cr),J9=rn(Ht)(h),j9=Q(),X9=M(X),Q9=Uu()(En),K9=qu(hi),Y9=W(ir),Z9=function(){return d.value}(),iw=function(t){return function(r){return function(e){return z9(Z9)({txt:C(`\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf
          OneOf.do
            bangOn
            delay 1000
              $ pure
              $ playbackRate
              $ AudioEnvelope
                  { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                  , o: 1.5
                  , d: 30.0
                  }
          )
      ]
  ]`),envelope:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([V9(1)([G9(u)(J9([j9,ga(1e3)(X9(Q9({p:K9(Y9(Ke(0)(60))([1,1.2,1,.8])),o:1.5,d:30})))]))])])}})})}}};var rQ=Yt({reflectType:function(){return`<section>
  <h2>AudioNumeric</h2>
  <p><code>AudioNumeric</code> encompasses the following three functions from the Web Audio API:</p>

  <ul>
    <li><code>linearRampToValueAtTime</code> via the <code>_linear</code> transition.</li>
    <li><code>exponentialRampToValueAtTime</code> via the <code>_exponential</code> transition.</li>
    <li><code>setValueAtTime</code> via the <code>_step</code> transition.</li>
  </ul>

  <p>Let's explore all of them in the example below.</p>

  <blockquote>Pro tip: When using <code>AudioNumeric</code>, consider starting with a <code>_step</code> transition. Otherwise, the transition may be abrupt and unpleasant!</blockquote>

  <pre><code>\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf OneOf.do
          bangOn
          delay 1000 OneOf.do
            pure
              $ playbackRate
              $ AudioNumeric { n: 1.0, o: 1.0, t: _step }
            pure
              $ playbackRate
              $ AudioNumeric { n: 1.3, o: 2.0, t: _linear }
          delay 2500 OneOf.do
            pure
              $ playbackRate
              $ AudioNumeric { n: 1.0, o: 2.5, t: _step }
            pure
              $ playbackRate
              $ AudioNumeric { n: 0.7, o: 3.5, t: _exponential }
      ]
  ]</code></pre>

  @numericEx@
  </section>
`}})({reflectType:function(){return"@"}})()()(q()(N)({reflectType:function(){return"numericEx"}})({reflectSymbol:function(){return"numericEx"}})),eQ=Pt(_t),nQ=tr(cr),gD=uD(qt),uQ=Q(),hD=M(X),SD=Uu()(Pa),aQ=function(){return d.value}(),ow=function(t){return function(r){return function(e){return rQ(d.value)(aQ)({numericEx:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([eQ(1)([nQ(u)(gD(uQ)(function(){return gD(ga(1e3)(gD(hD(SD({n:1,o:1,t:Dk})))(function(){return hD(SD({n:1.3,o:2,t:Ji}))})))(function(){return ga(2500)(gD(hD(SD({n:1,o:2.5,t:Dk})))(function(){return hD(SD({n:.7,o:3.5,t:G0}))}))})}))])])}})})}}};var oQ=Yt({reflectType:function(){return`<section>
  <h2>AudioSudden</h2>
  <p>The simplest change you can make is scheduling a value to change <i>now</i>. This is done with <code>AudioSudden</code>, which is a wrapper around the setter for an audio parameter's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/value"><code>value</code></a> field in the Web Audio API.</p>

  <p>In the example below, we change a value after it has run for 1.5 seconds.</p>

  <pre><code>\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf OneOf.do
          bangOn
          delay 1500
            $ pure
            $ playbackRate
            $ AudioSudden { n: 1.4 }
      ]
  ]</code></pre>

  @suddenEx@
  </section>
`}})({reflectType:function(){return"@"}})()()(q()(N)({reflectType:function(){return"suddenEx"}})({reflectSymbol:function(){return"suddenEx"}})),fQ=Pt(_t),cQ=tr(cr),lQ=rn(Ht)(h),pQ=Q(),_Q=M(X),vQ=Uu()(z0),sQ=function(){return d.value}(),fw=function(t){return function(r){return function(e){return oQ(d.value)(sQ)({suddenEx:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([fQ(1)([cQ(u)(lQ([pQ,ga(1500)(_Q(vQ({n:1.4})))]))])])}})})}}};var DQ=Dt({reflectType:function(){return`<section>
  <h2>Audio Units</h2>
  <p>In my humble opinion, the summit of Web Audio programming is when audio units control the audio parameters of other audio units. This allows for a form of radical experimentation that is difficult in many other frameworks. <a href="https://www.w3.org/TR/webaudio/#ModularRouting">Nearly any audio parameter</a> can be automated this way.</p>

  <p>To control an audio parameter with an audio unit, use the <code>AudioUnit</code> constructor. You can also use a <code>Node D1 l p</code>. If your node is for an arbitrary number of channels, make sure to coerce it to mono using the <code>c1</code> function, as in the example below.</p>

  <pre><code>\\ctx buf -> run2 ctx
  [ loopBuf buf OneOf.do
      bangOn
      pure
        $ playbackRate
        $ c1
            ( gain_ 1.0
                [ constant 1.0 bangOn
                , gain_ 0.2 (lowpass_ 100.0 (squareOsc 50.0 bangOn))
                ]
            )
      )
  ]
</code></pre>

  ~unitEx~
  </section>
`}})()()(q()(N)({reflectType:function(){return"unitEx"}})({reflectSymbol:function(){return"unitEx"}})),bQ=tr(cr),AQ=rn(Ht)(h),vg=Q(),kQ=M(X),dQ=Uu()(W0(lc)),cw=Pt(_t),yQ=Sm(vm),gQ=Ll(bm),hQ=L_(Tl),SQ=function(){return d.value}(),lw=function(t){return function(r){return function(e){return DQ(SQ)({unitEx:vt(e)(t)(function(n){return at(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([bQ(u)(AQ([vg,kQ(dQ(B0(cw(1)([yQ(1)(vg),cw(.2)([gQ(100)([hQ(50)(vg)])])]))))]))])}})})}}};var CQ=rr(or),yv=q(),$Q=Dt({reflectType:function(){return`<div>
  <h1>Parameters</h1>

  <h3>Controlling our units</h3>
  <p>
    In the previous section, we saw how we can use browser events to control audio units. The Web Audio API provides a rich set of tools to control both the audio-rate and control-rate parameters of audio units. This section goes over how ocarina exposes those parameters.
  </p>

  ~sudden~
  ~numeric~
  ~envelope~
  ~cancel~
  ~unit~

  <h2>Next steps</h2>
  <p>In this section, we saw how to specify parameters for audio units, including using audio-rate audio units as parameters. In the next section, we'll look at how to make events <a ~next~ style="cursor:pointer;">stateful</a>.</p>
</div>`}})()()(yv(yv(Je()(yv(yv(yv(N)({reflectType:function(){return"unit"}})({reflectSymbol:function(){return"unit"}}))({reflectType:function(){return"sudden"}})({reflectSymbol:function(){return"sudden"}}))({reflectType:function(){return"numeric"}})({reflectSymbol:function(){return"numeric"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}}))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}})),TQ=ou(qt)(X),FQ=function(){return d.value}(),pw=function(t){return function(r){return function(e){return function(n){var u=CQ(r(H_.value))(Xe),a=fu(t)(e);return $Q(FQ)({sudden:fw(a)(r)(n),numeric:ow(a)(r)(n),envelope:iw(a)(r)(n),cancel:uw(a)(r)(n),unit:lw(a)(r)(n),next:TQ(n)(u)})}}}};var MQ=Dt({reflectType:function(){return`<div>
  <h1>Tumult</h1>

  <h2>The unknown unknowns of web audio</h2>
  <p>
    As we saw in the last section on subgraphs, sometimes, you simply don't know how your audio will evolve. For example, if we are building a live coding environment and create a space where someone can basically do <i>anything</i>, working with anonymous audio nodes will not cut it. For example, imagine that they have two relatively unconnected parts of the audio graph and want to start creating cross-connections. With subgraphs, we can't pick apart the graph and say "take node X and connect it to node Y" as nodes don't have names.
  </p>

  <p>
    Tumult solves this problem. It is an entirely separate rendering engine from the one we've seen before that works via <i>diffing</i> two audio graphs and adjusting the current graph based on the diff. The biggest difference is that tumult <i>requires</i> you to give a name to each audio unit. Otherwise, the engine won't know what node corresponds to what name.
  </p>

  <p>
    For you veteran coders and audio-ers, you may bristle at the word <i>diff</i>. After all, diffing is expensive, and we are working in resource constrained environments where timing is of the essence. Fear not! Tumult uses PureScript <code>Set</code>-s to do blazingly fast diffs. That said, tumult is the slowest part of ocarina (meaning it is just blazingly fast instead of obscenely and indecently fast), so only use it where you can afford a performmance hit.
  </p>

  <h2>Hello tumult</h2>

  <h2>Stability in tumult</h2>

  <p>While tumult will run a diff every time its event is triggered, if you don't tear down a node, its nodes will merrily putter along with whatever events you send them. This means that you can trigger tumult events <i>only</i> when you need to change the shape of the graph.</p>

  <h2>Feeling lucky</h2>

  <p>So far, we have only seen the <code>DiffMe</code> instruction sent to tumult. There is another instruction you can send called <code>FeelingLucky</code>. <code>FeelingLucky</code> will attempt to perform its instruction and be a no-op if it is incoherent with respect to the graph. Careful with this, though! <code>DiffMe</code> guarantees that whatever you send to tumult will be what's rendered from your loudspeaker. Too many <code>FeelingLucky</code>-s, on the other hand, can lead to bugs where you're not quite sure anymore <i>what</i> is playing. That said, <code>FeelingLucky</code> is grotesquely fast, especially for larger graphs. So if it makes sense to use it, use it!</p>

  <h2>Next steps</h2>
  <p>In this section, we learned how to use tumult to create truly dynamic audio graphs that allow you to fine-tune the speed-versus-dynamism tradeoff in various ways.</p>
  <p>In the next section, we'll look at how to create audio graphs via an <a ~next~ style="cursor:pointer;">imperative API that more closely resembles Web Audio while providing additional type-safety benefits</a>.</p>
</div>`}})()()(Je()(N)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})),OQ=M(X),wQ=mt(ke),PQ=rr(or),IQ=function(){return d.value}(),_w=function(t){return function(r){return function(e){return function(n){return MQ(IQ)({next:OQ(wQ(ee.value)(Vr(w(PQ(r(Cm.value))(Xe)))))})}}}};var LQ=Dt({reflectType:function(){return`<div>
  <h1>Audio Worklets</h1>

  <h2>Supercharging our audio experiences</h2>

  <p>The Web Audio lets you do basically anything, but when anything isn't enough, there are custom Audio Worklets. </p>

  <p>Custom audio worklets come in a few shapes an sizes:</p>

  <ul>
    <li>You can <a href="https://developer.chrome.com/blog/audio-worklet/">write your own in JavaScript</a>. While this is ok for small PoCs, it will get really slow really quick.</li>
    <li>You can compile your favorite C/C++/Rust audio processors to <a href="https://developer.chrome.com/blog/audio-worklet-design-pattern/#using-audio-worklet-with-webassembly">web assembly</a>. The helpful folks at Google have created many examples that do exactly this, and you can pilfer them for fun and profit.</li>
    <li>My go-to solution is Faust. <a href="https://faust.grame.fr/">Faust</a> is the greatest audio project on Earth, and let's you build very performant audio processors with an expressive and concise syntax. Faust's <a href="https://webaudioconf.com/posts/2017_EA_60/">Web Audio integration</a> is a one-liner from the command line.</li>
  </ul>

  <p>In this example, we'll use Faust to create a three custom audio units and wire them together using Ocarina. The units are:</p>

  <ul>
    <li>A wicked sawtooth synth.</li>
    <li>A kickin' comb filter.</li>
    <li>Freeverb, the free-software-world's favorite reverb!</li>
  </ul>

  <h2>Faust</h2>

  <p>Here are our three faust examples:</p>

  <h3>Sawtooth</h3>

  <h3>Comb filter</h3>

  <h3>Freeverb</h3>

  <h3>Compilation</h3>

  We can compile each example using the following commands. Each command creates a self-contained JavaScript file, so all we need to do is link to it in ocarina-land.

  <pre><code>placeholder</code></pre>

  <h2>Ocarina</h2>

  <p>Ocarina provides a type-safe interface for declaring the API of Audio Worklets. While it's your job to make sure the API is in fact the actual API of the worklet, assuming this is correct, ocarina will enforce it for you. Let's see how in the following example. Here's the code:</p>

  <pre><code>placeholder</code></pre>

  <p>And here's the result:</p>

  <blockquote>placeholder</blockquote>

  <p>You can try it yourself by <a>following this trypurescript link.</a></p>

  <h3>The API contract</h3>

  <p>The <code>InitializeAudioWorklet</code> API contract is defined like so:</p>

  <pre><code>InitializeAudioWorkletNode
  { name :: Proxy name
  , numberOfInputs :: numberOfInputs
  , numberOfOutputs :: numberOfOutputs
  , outputChannelCount :: outputChannelCount
  , parameterData :: { | parameterData }
  , processorOptions :: { | processorOptions }
  }</code></pre>

  <p>And here is how the APIs parameters are used:</p>

  <table>
    <tr>
      <th>Parameter</th>
      <th>Description</th>
    </tr>
    <tr>
      <td><code>name</code></td>
      <td>The name of the worklet as defined in the JS output. When in doubt, check the JS produced by faust!</td>
    </tr>
    <tr>
      <td><code>numerOfInputs</code></td>
      <td>How many inputs does the node take? Note that this is <i>not</i> the number of channels, but rather the number of distinct inputs, like in a merger node.</td>
    </tr>
    <tr>
      <td><code>numerOfOutputs</code></td>
      <td>How many outputs does the node produce? Note that this is <i>not</i> the number of channels, but rather the number of distinct outputs, like in a splitter node.</td>
    </tr>
    <tr>
      <td><code>outputChannelCount</code></td>
      <td>How many channels are in each output? This is defined using a tuple of type-level numbers.</td>
    </tr>
    <tr>
      <td><code>parameterData</code></td>
      <td>Initial parameters for the audio node. This will also define what parameters can be sent to it via events (the record gets flipped to a <a href="https://github.com/natefaubion/purescript-variant"><code>Variant</code></a> when it is an event).</td>
    </tr>
  </table>

  <h2>Next steps</h2>
  <p>In this section, we created three audio worklet nodes using Faust and used them in the Web Audio API via ocarina. There is active work going on to bundle all this into a single toolchain so that Faust can be written directly in PureScript and automatically read as an Audio Worklet by ocarina. Until that happens, though, this is a great solution: just make sure to get the parameter names right across the language barrier! No amonut of type-safety can save you there \u{1F605}</p>
  <p>In the next section, we'll look at how to create <a ~next~ style="cursor:pointer;">mutable state in a ocarina graph</a>.</p>
</div>`}})()()(Je()(N)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})),NQ=M(X),UQ=mt(ke),BQ=rr(or),WQ=function(){return d.value}(),vw=function(t){return function(r){return function(e){return function(n){return LQ(WQ)({next:NQ(UQ(ee.value)(Vr(w(BQ(r(Sc.value))(Xe)))))})}}}};var HQ=Yt({reflectType:function(){return`<div>
  <h1>Imperative API</h1>

  <h2>Like JavaScript, but PureScript</h2>
  <p>
    If you're coming from the JavaScript or TypeScript world, or if you're a fan of monadic <code>do</code> notation, you may enjoy building things step-by-step rather than constructing large declarative structures. If you're that sort of person, this section is for you!
  </p>

  <h2>Parting shot</h2>
  <p>Thanks for checking out ocarina! We want it to be the most ergonomimc, expressive, and performant Web Audio API on your side of the Mississippi. It certainly is for me, and as I'm in Finland, I'm on <i>both sides</i> of the Mississippi, so you can't beat that! If you have any questions, comments, concerns or would just like to say "hi!", please check out the <a href="https://github.com/mikesol/purescript-ocarina">Ocarina GitHub Repo</a> or the <a href="https://purescript.org/chat">PureScript Discord's music channel</a>. Now go out there and play some ocarina!</p>
</div>`}})({reflectType:function(){return"~"}})()()(N),zQ=function(){return d.value}(),sw=function(t){return function(r){return function(e){return function(n){return HQ(d.value)(zQ)({})}}}};var mw=q(),GQ=Yt({reflectType:function(){return`<section>
  <h2>Hello subgraph</h2>

  <p>Subgraphs have the type <code>Event (Event (Channel outputChannels payload))</code>. Streaming audio is a data type with two constructors: <code>sound</code> to create a subgraph and <code>silence</code> to turn it off. The inner event listens for sound/silence, and the outer event adds subgraphs to the scene. You can create as many subgraphs as you like: ocarina automatically frees up resources when you send the <code>silence</code> event. Note that, once you turn a subraph off with <code>silence</code>, you can't turn it back on again. In this case, just create a new subgraph.</p>

  <p>Here's a simple subgraph that is connected to a slider. As you slide the slider, new nodes are provisioned. Each one has a pseudo-random pitch.</p>

  <pre><code>@txt@</code></pre>
  @ex1@

</section>
`}})({reflectType:function(){return"@"}})()()(mw(mw(N)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})),Dw=ht(qt),gv=M(X),JQ=No(ct),jQ=da(me),XQ=Pt(_t),hv=S(ct),bw=rn(Ht)(h),QQ=au(w_(Zt(Cr()(dt(dt(xr)(nT)()()()({reflectSymbol:function(){return"playbackRate"}}))(O_)()()()({reflectSymbol:function(){return"buffer"}})))(Sr()()))),KQ=Q(),Aw=_u(Ht)(h),YQ=mt(Zi),ZQ=mt(Uf),tK=mt(Nf),rK=mt(Wf),eK=mt(qf),nK=mt(Bf),uK=mt(ke),kw=W(ct),sg=M(Wt),dw=oe(ct),xD=rr(or),aK=it(pn),iK=K(Z),mg=lt(qn),oK=Mn(Ge),fK=Na(Ge),cK=de(Ge),lK=mn(ne),pK=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst)
import QualifiedDo.Alt as OneOf
import Data.Tuple.Nested ((/\\))
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (switcher, text, text_)
import Deku.Core (Nut)
import Bolson.Core (envy)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Random as Random
import FRP.Behavior (Behavior, behavior, sampleBy)
import FRP.Event (create, fold, makeEvent, subscribe, delay)

import FRP.Event.VBus (V, vbus)
import QualifiedDo.OneOfMap as O
import Type.Proxy (Proxy(..))
import Ocarina.Control (gain_, playBuf)
import Ocarina.Core (Channel(..), dyn, bangOn)
import Ocarina.Interpret (bracketCtx, decodeAudioDataFromUri)
import Ocarina.Run (run2_)
import Ocarina.WebAPI (BrowserAudioBuffer)

type StartStop = V (start :: Unit, stop :: Effect Unit)
type UIEvents = V (startStop :: StartStop, slider :: Unit)

bell =
  "https://freesound.org/data/previews/339/339810_5121236-lq.mp3"
    :: String

random :: Behavior Number
random = behavior \\e ->
  makeEvent \\k -> subscribe e \\f ->
    Random.random >>= k <<< f

main :: Effect Unit
main = do
  { push, event } <- create
  runInBody (switcher scene event)
  push Nothing
  launchAff_ $ bracketCtx
    \\ctx -> decodeAudioDataFromUri ctx bell >>= liftEffect
      <<< push
      <<< Just
  where
  scene
    :: forall payload
     . Maybe BrowserAudioBuffer
    -> Nut Effect payload
  scene = maybe (D.div_ [ text_ "Loading..." ]) \\buffer ->
    D.div_ $ pure $ envy $ vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        startE = pure unit <|> event.startStop.start
        sl = sampleBy (/\\) random
          $ fold (\\_ b -> b + 1) event.slider 0
        music = run2_
          [ gain_ 1.0
              [ dyn $ map
                  ( \\i ->
                      OneOf.do
                        pure $ sound $ playBuf
                          { buffer: buffer, playbackRate: 0.7 + (fst i) * 2.0 }
                          bangOn
                        delay 5000 $ pure $ silence
                  )
                  sl
              ]
          ]
      D.div_
        [ D.div_
            [ text_ "Slide me!"
            , D.input
                ( O.oneOfMap pure O.do
                    D.Xtype := "range"
                    D.Min := "0"
                    D.Max := "100"
                    D.Step := "1"
                    D.Value := "50"
                    D.OnInput := cb (const (push.slider unit))
                )
                []
            ]
        , D.button
            ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                startE $> (music >>= push.startStop.stop)
                event.startStop.stop <#>
                  (_ *> push.startStop.start unit)
            )
            [ text OneOf.do
                startE $> "Turn on"
                event.startStop.stop $> "Turn off"
            ]
        ]
`,_K=Bu(function(t){return Cn(function(r){return we(t)(function(e){return function(){var u=Ui();return r(e(u))()}})})}),vK=function(){return d.value}(),sK="https://freesound.org/data/previews/339/339810_5121236-lq.mp3",yw=function(t){return function(r){return function(e){return GQ(d.value)(vK)({txt:C(pK),ex1:Li(Dl)(function(n){return Li(Dl)(function(u){return Li(Dl)(function(a){return Li(Dl)(function(o){var i=Dw(gv(void 0))(n.value1),f=JQ(U.create)(_K)(jQ(function(v){return function(D){return v+1|0}})(0)(o.value1)),m=function(v){return[XQ(1)([Zp(hv(function(D){return bw([gv(N0(QQ({buffer:v,playbackRate:.7+pu(D)*2})(KQ))),ga(5e3)(gv(U0))])})(f))])]};return Lr([Lr([C("Slide me!"),yi(Aw(gv)([YQ(di.value)("range"),ZQ(Wo.value)("0"),tK(Bo.value)("100"),rK(Ho.value)("1"),eK(zo.value)("50"),nK(qo.value)(Vr(w(o.value0(void 0))))]))([])]),on(Aw(hv(function(){var v=uK(ee.value);return function(D){return v(Vr(w(D)))}}()))([kw(a.value1)(sg(void 0)),dw(u.value1)(function(v){return xD(v)(xD(t(sg(void 0)))(n.value0(void 0)))}),dw(aK(kw(i)(iK))(Dw(gv(sg(void 0)))(hv(function(v){return v.value0})(e))))(function(v){return function(){v(),a.value0(void 0)();var A=Ca(mg(oK)(function(b){return mg(fK(b))(function(_){return mg(at(b)(sK))(function(k){return cK(function(){var nt=wm(m(k))(),bt=xD(xD(nt)(_))(lK(b));return u.value0(bt)(),bt})})})}))();return t(function(){return n.value0(void 0)(),ii(Ro(A))()})(),void 0}})]))([Ve(bw([hv(w("Turn off"))(u.value1),hv(w("Turn on"))(i)]))])])})})})})})}}};var gw=q(),DK=Dt({reflectType:function(){return`<div>
  <h1>Subgraphs</h1>

  <h2>Making audio even more dynamic</h2>
  <p>
    When we're creating video games or other types of interactive work, it's rare that we'll be able to anticipate the exact web audio graph we'll need for an entire session. As an example, imagine that in a video game a certain sound effects accompany various characters, and those characters come in and out based on your progress through the game. One way to solve this would be to anticipate the maximum number of characters that are present in a game and do a round-robin assignment of nodes in the audio graph as characters enter and leave your game. But sometimes that's not ergonomic, and in certain cases its downright inefficient. Another downside is that it does not allow for specialization of the web audio graph based on new data, like for example a character to play a custom sound once you've earned a certain badge.
  </p>

  <p>
    Subgraphs fix this problem. They provide a concise mechansim to dynamically insert audio graphs based on events.
  </p>

  ~suby~

  <h2>Go forth and be brilliant!</h2>
  <p>Thus ends the first version of the ocarina documentation. Applause is always welcome ~appl~! Alas, some features remain undocumented, like audio worklets and an imperative API. At some point I hope to document all of these, but hopefully this should be enough to get anyone interested up and running. If you need to use any of those features before I document them, ping me on the <a href="https://purescript.org/chat">PureScript Discord</a>. Otherwise, happy music making with Ocarina!</p>
</div>`}})()()(gw(gw(N)({reflectType:function(){return"suby"}})({reflectSymbol:function(){return"suby"}}))({reflectType:function(){return"appl"}})({reflectSymbol:function(){return"appl"}})),bK=Pt(_t),AK=tr(cr),kK=Q(),dK=function(){return d.value}(),hw=function(t){return function(r){return function(e){return function(n){var u=fu(t)(e);return DK(dK)({appl:Om("\u{1F44F}")(n)(u)(function(a){return at(a)("https://freesound.org/data/previews/277/277021_1402315-lq.mp3")})(function(a){return function(o){return Y(a)([bK(1)([AK(o)(kK)])])}}),suby:yw(u)(r)(n)})}}}};var Wa=M(Ha),gK=da(me),hK=M(Wt),SK=S(ir),xK=ht(qt),CK=_u(Ht)(h),Sw=M(X),xw=mt(ke),$K=mt(qx),TK=S(ct),Cw=qp(ya),FK=mt(bs),$w=Ut(Kf(io(uo))),OSr=function(t){return t},wSr={Coercible0:function(){}},EK=function(){var t=function(r){var e=function(n){if(n instanceof U_)return Lr(Wa(Ue(ZO(r.setCancellation)(r.setPage))));if(n instanceof hc)return Lr(Wa(Ue(OO(r.setCancellation)(r.setPage))));if(n instanceof W_)return Lr(Wa(Ue(TO(r.setCancellation)(r.setPage))));if(n instanceof B_)return Lr(Wa(Ue($E(r.setCancellation)(r.setPage))));if(n instanceof xm)return Lr(Wa(Ue(vw(r.setCancellation)(r.setPage))));if(n instanceof Sc)return Lr(Wa(Ue(DO(r.setCancellation)(r.setPage))));if(n instanceof q_)return Lr(Wa(Ue(pw(r.setCancellation)(r.setPage))));if(n instanceof H_)return Lr(Wa(Ue(NM(r.setCancellation)(r.setPage))));if(n instanceof Cm)return Lr(Wa(Ue(sw(r.setCancellation)(r.setPage))));if(n instanceof oF)return Lr(Wa(Ue(tw(r.setCancellation)(r.setPage))));if(n instanceof z_)return Lr(Wa(Ue(hw(r.setCancellation)(r.setPage))));if(n instanceof fF)return Lr(Wa(Ue(_w(r.setCancellation)(r.setPage))));throw new Error("Failed pattern match at Ocarina.Example.Docs (line 140, column 5 - line 140, column 76): "+[n.constructor.name])};return e(r.page)};return Li(yC(new Nl(U_.value)))(function(r){var e=gK(Jt(function(n){if(n instanceof Nl)return function(u){return{prevPage:new F(u.curPage),curPage:n.value0,cancel:u.cancel,pageChange:!0}};if(n instanceof Bk)return function(u){return{cancel:n.value0,pageChange:!1,curPage:u.curPage,prevPage:u.prevPage}};throw new Error("Failed pattern match at Ocarina.Example.Docs (line 51, column 13 - line 53, column 81): "+[n.constructor.name])}))({prevPage:P.value,curPage:U_.value,cancel:hK(void 0),pageChange:!0})(r.value1);return Lr([Lr(SK(function(n){return fA([oA(xK(CK(Sw)([xw(ee.value)(Vr(w(r.value0(new Nl(n.value0))))),$K(Ot.value)("cursor:pointer;")]))(TK(function(u){return xw(ee.value)(Vr(w(function(){return u.cancel(),r.value0(new Nl(n.value0))()})))})(Cw(function(u){return!function(a){return a.pageChange}(u)})(e))))([C(n.value1.value0)]),cl(Sw(FK(Ot.value)(function(){return n.value1.value1?"":"display:none;"}())))([C(" | ")])])})([new U(U_.value,new U("Home",!0)),new U(hc.value,new U("Hello world",!0)),new U(W_.value,new U("Array, fan, and fix",!0)),new U(B_.value,new U("Audio units",!0)),new U(Sc.value,new U("Events",!0)),new U(q_.value,new U("Parameters",!0)),new U(H_.value,new U("State",!0)),new U(z_.value,new U("Subgraphs",!1))])),Rx(function(n){return t({page:n.curPage,setPage:function(u){return r.value0(Nl.create(u))},setCancellation:function(u){return r.value0(Bk.create(u))}})})(Cw(function(n){return n.pageChange})(e))])})}(),PSr=function(t){return{page:t,setPage:$w,setCancellation:$w}},ISr=V$(EK);export{OSr as TopLevelSg,ISr as main,wSr as newtypeTopLevelSg_,PSr as p2tl,EK as scene};
