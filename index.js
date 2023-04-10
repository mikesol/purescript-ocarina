var bg=function(t){return function(r){for(var e=r.length,n=new Array(e),u=0;u<e;u++)n[u]=t(r[u]);return n}};var gi={compose:function(t){return function(r){return function(e){return t(r(e))}}}},fa=function(t){return t.compose},ip=function(t){var r=fa(t);return function(e){return function(n){return r(n)(e)}}};var Q=function(t){return t.identity},Z={identity:function(t){return t},Semigroupoid0:function(){return gi}};var Jr=!0;var Gt=function(t){return function(r){return function(e){return t(e)(r)}}},M=function(t){return function(r){return t}};var Jf=function(t){return function(r){return r(t)}},Mc=function(t){return function(r){return t(r)}};var d=function(){function t(){}return t.value=new t,t}();var g=function(t){return t.map},oe=function(t){var r=g(t);return function(e){return function(n){return r(n)(e)}}},Ir=function(t){return g(t)(M(void 0))},B=function(t){var r=g(t);return function(e){return function(n){return r(M(n))(e)}}},xv=function(t){var r=g(t);return function(e){return r(M(e))}};var Qe={map:fa(gi)},ir={map:bg},$D=function(t){var r=g(t);return function(e){return function(n){return r(function(u){return u(n)})(e)}}};var Ag=function(t){return function(r){return t+r}},kg=function(t){return function(r){return t.length===0?r:r.length===0?t:t.concat(r)}};var fe=function(t){return t.reflectSymbol};var Jo=function(t){var r=function(e){var n;function u(a){e=a}for(;;)n=u(e);return n};return r(t)};var Cv=function(t){return function(r){return{}.hasOwnProperty.call(r,t)}},Au=function(t){return function(r){return r[t]}},qa=function(t){return function(r){return function(e){var n={};for(var u in e)({}).hasOwnProperty.call(e,u)&&(n[u]=e[u]);return n[t]=r,n}}};var dg={append:function(t){return function(r){return void 0}}},$v={append:Ag};var Ne={append:kg};var K=function(t){return t.append},TD=function(t){var r=K(t);return{append:function(e){return function(n){return function(u){return r(e(u))(n(u))}}}}};var FD={alt:K(Ne),Functor0:function(){return ir}},gt=function(t){return t.alt};var yg=function(t){return function(r){for(var e=t.length,n=r.length,u=new Array(e*n),a=0,o=0;o<e;o++)for(var i=t[o],f=0;f<n;f++)u[a++]=i(r[f]);return u}};var Rw=Q(Z);var fp={apply:yg,Functor0:function(){return ir}},at=function(t){return t.apply};var rr=function(t){var r=at(t),e=g(t.Functor0());return function(n){return function(u){return r(e(M(Rw))(n))(u)}}},$e=function(t){var r=at(t),e=g(t.Functor0());return function(n){return function(u){return function(a){return r(e(n)(u))(a)}}}};var F=function(t){return t.pure};var In=function(t){var r=F(t);return function(e){return function(n){if(e)return n;if(!e)return r(void 0);throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): "+[e.constructor.name,n.constructor.name])}}},cp=function(t){var r=at(t.Apply0()),e=F(t);return function(n){return function(u){return r(e(n))(u)}}};var Ha={pure:function(t){return[t]},Apply0:function(){return fp}};var gg=function(t){return function(r){for(var e=[],n=0,u=t.length;n<u;n++)Array.prototype.push.apply(e,r(t[n]));return e}};var Bw=Q(Z);var hi={bind:gg,Apply0:function(){return fp}},ct=function(t){return t.bind},cn=function(t){return Gt(ct(t))};var Oc=function(t){var r=ct(t);return function(e){return function(n){return function(u){return r(e(u))(n)}}}};var qu=function(t){var r=ct(t);return function(e){return r(e)(Bw)}};var Ke=function(t){return function(r){for(var e=t>r?-1:1,n=new Array(e*(r-t)+1),u=t,a=0;u!==r;)n[a++]=u,u+=e;return n[a]=u,n}},Ww=function(t){return function(r){if(t<1)return[];var e=new Array(t);return e.fill(r)}},qw=function(t){return function(r){for(var e=[],n=0,u=0;u<t;u++)e[n++]=r;return e}},Tv=typeof Array.prototype.fill=="function"?Ww:qw,Hw=function(){function t(u,a){this.head=u,this.tail=a}var r={};function e(u){return function(a){return new t(u,a)}}function n(u){for(var a=[],o=0,i=u;i!==r;)a[o++]=i.head,i=i.tail;return a}return function(u){return function(a){return n(u(e)(r)(a))}}}(),Vn=function(t){return t.length};var hg=function(t){return function(r){return function(e){return function(n){return n<0||n>=e.length?r:t(e[n])}}}};var Sg=function(t){return function(r){return function(e){return function(n){for(var u=0,a=n.length;u<a;u++)if(e(n[u]))return t(u);return r}}}};var xg=function(t){return function(r){return function(e){return function(n){if(e<0||e>=n.length)return r;var u=n.slice();return u.splice(e,1),t(u)}}}};var zw=function(){function t(r,e,n,u,a,o){var i,f,m,v,D,A,b;for(i=a+(o-a>>1),i-a>1&&t(r,e,u,n,a,i),o-i>1&&t(r,e,u,n,i,o),f=a,m=i,v=a;f<i&&m<o;)D=u[f],A=u[m],b=e(r(D)(A)),b>0?(n[v++]=A,++m):(n[v++]=D,++f);for(;f<i;)n[v++]=u[f++];for(;m<o;)n[v++]=u[m++]}return function(r){return function(e){return function(n){var u;return n.length<2?n:(u=n.slice(0),t(r,e,u,n.slice(0),0,n.length),u)}}}}();var wc=function(t){return function(r){return function(e){for(var n=r.length<e.length?r.length:e.length,u=new Array(n),a=0;a<n;a++)u[a]=t(r[a])(e[a]);return u}}};var Cg=function(t){return function(r){return t[r]}};var Gw=function(t,r,e){var n=0,u;return function(a){if(n===2)return u;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+a+")",r,a);return n=1,u=e(),n=2,u}};var $g={defer:function(t){return function(r){return t(void 0)(r)}}},Ic=function(t){return t.defer},OD=function(t){var r=Ic(t);return function(e){var n=Gw("go","Control.Lazy",function(){return r(function(a){return e(n(25))})}),u=n(25);return u}};var Gn=function(t){var r=ct(t.Bind1()),e=F(t.Applicative0());return function(n){return function(u){return r(n)(function(a){return r(u)(function(o){return e(a(o))})})}}};var Jw=String.fromCharCode(65535),jw=String.fromCharCode(0),Xw=Number.POSITIVE_INFINITY,Qw=Number.NEGATIVE_INFINITY;var Tg=function(t){return function(r){return function(e){return function(n){return function(u){return n<u?t:n===u?r:e}}}}};var Fg=Tg,Eg=Tg;var Mg=function(t){return function(r){return t===r}};var Og=Mg,wg=Mg;var lp={eq:wg},eo={eq:Og};var dt=function(t){return t.eq};var It=function(){function t(){}return t.value=new t,t}(),Jt=function(){function t(){}return t.value=new t,t}(),Xt=function(){function t(){}return t.value=new t,t}();var Ig=function(t){return function(r){return t-r|0}},Pg=function(t){return function(r){return t-r}};var Lg=function(t){return function(r){return t+r|0}},Rg=function(t){return function(r){return t*r|0}},Ng=function(t){return function(r){return t+r}},Ug=function(t){return function(r){return t*r}};var gn=function(t){return t.zero};var Jn={add:Ng,zero:0,mul:Ug,one:1},pa={add:Lg,zero:0,mul:Rg,one:1};var Pn=function(t){return t.one};var Ye=function(t){return t.mul};var Er=function(t){return t.add};var du=function(t){return t.sub};var jf={sub:Pg,Semiring0:function(){return Jn}},wD={sub:Ig,Semiring0:function(){return pa}};var pp=function(t){var r=du(t),e=gn(t.Semiring0());return function(n){return r(e)(n)}};var za=function(){return{compare:Eg(It.value)(Xt.value)(Jt.value),Eq0:function(){return lp}}}(),Te=function(){return{compare:Fg(It.value)(Xt.value)(Jt.value),Eq0:function(){return eo}}}();var Pt=function(t){return t.compare};var zg=function(t){var r=Pt(t);return function(e){return function(n){var u=r(e)(n);return!(u instanceof It)}}};var Xo=function(t){var r=Pt(t);return function(e){return function(n){var u=r(e)(n);if(u instanceof It)return n;if(u instanceof Xt||u instanceof Jt)return e;throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): "+[u.constructor.name])}}};var PD=function(t){var r=zg(t);return function(e){var n=gn(e.Semiring0()),u=pp(e);return function(a){var o=r(a)(n);return o?a:u(a)}}};var Ln=function(t){return t.top};var vp={top:2147483647,bottom:-2147483648,Ord0:function(){return Te}};var Rn=function(t){return t.bottom};var Gg=function(t){return t.toString()};var gu={show:Gg};var Ft=function(t){return t.show};var v1=Q(Z),w=function(){function t(){}return t.value=new t,t}(),$=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var yt=function(t){return function(r){return function(e){if(e instanceof w)return t;if(e instanceof $)return r(e.value0);throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}};var Ae={map:function(t){return function(r){return r instanceof $?new $(t(r.value0)):w.value}}},s1=g(Ae);var Sn=function(t){return yt(t)(v1)},xn=function(){return function(t){if(t instanceof $)return t.value0;throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): "+[t.constructor.name])}};var Ko={apply:function(t){return function(r){if(t instanceof $)return s1(t.value0)(r);if(t instanceof w)return w.value;throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): "+[t.constructor.name,r.constructor.name])}},Functor0:function(){return Ae}};var Nn={bind:function(t){return function(r){if(t instanceof $)return r(t.value0);if(t instanceof w)return w.value;throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): "+[t.constructor.name,r.constructor.name])}},Apply0:function(){return Ko}};var no=function(){return{pure:$.create,Apply0:function(){return Ko}}}();var St=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),xt=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var Qf={map:function(t){return function(r){if(r instanceof St)return new St(r.value0);if(r instanceof xt)return new xt(t(r.value0));throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): "+[r.constructor.name])}}};var jn=function(t){return function(r){return function(e){if(e instanceof St)return t(e.value0);if(e instanceof xt)return r(e.value0);throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},Fv=function(){return jn(M(w.value))($.create)}();var Hu=function(t){return t};var Va={map:function(t){return function(r){return t(r)}}};var Jg={apply:function(t){return function(r){return t(r)}},Functor0:function(){return Va}},m1={bind:function(t){return function(r){return r(t)}},Apply0:function(){return Jg}},RD={pure:Hu,Apply0:function(){return Jg}},_a={Applicative0:function(){return RD},Bind1:function(){return m1}};var jg=function(t){return Math.min(Math.abs(t),2147483647)},Xg=function(t){return function(r){return r===0?0:r>0?Math.floor(t/r):-Math.floor(t/-r)}},Qg=function(t){return function(r){if(r===0)return 0;var e=Math.abs(r);return(t%e+e)%e}},Kg=function(t){return function(r){return t/r}};var Yg={Ring0:function(){return jf}},Zg={Ring0:function(){return wD}};var hu=function(t){return t.mod};var Ev={degree:function(t){return 1},div:Kg,mod:function(t){return function(r){return 0}},CommutativeRing0:function(){return Yg}},Ga={degree:jg,div:Xg,mod:Qg,CommutativeRing0:function(){return Zg}},Ja=function(t){return t.div};var uo={mempty:void 0,Semigroup0:function(){return dg}};var Nt=function(t){return t.mempty},Kf=function(t){var r=Nt(t),e=TD(t.Semigroup0());return{mempty:function(n){return r},Semigroup0:function(){return e}}};var ND=function(t){return function(){return t}},th=function(t){return function(r){return function(){return r(t())()}}};var bp=function(t){return function(r){return function(){for(var e=0,n=t.length;e<n;e++)r(t[e])()}}};var rh=function(t,r,e){var n=0,u;return function(a){if(n===2)return u;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+a+")",r,a);return n=1,u=e(),n=2,u}},ao={Applicative0:function(){return Bt},Bind1:function(){return Qn}},Qn={bind:th,Apply0:function(){return UD(0)}},Bt={pure:ND,Apply0:function(){return UD(0)}},eh=rh("functorEffect","Effect",function(){return{map:cp(Bt)}}),UD=rh("applyEffect","Effect",function(){return{apply:Gn(ao),Functor0:function(){return eh(0)}}}),L=eh(20),or=UD(23),k1=$e(or),BD=function(t){return{append:k1(K(t))}},io=function(t){var r=BD(t.Semigroup0());return{mempty:ND(Nt(t)),Semigroup0:function(){return r}}};var nh=function(t){return function(){return{value:t}}};var Ur=function(t){return function(){return t.value}},uh=function(t){return function(r){return function(){var e=t(r.value);return r.value=e.state,e.value}}},te=function(t){return function(r){return function(){r.value=t}}};var y1=Ir(L),Or=nh,g1=uh,Yf=function(t){return g1(function(r){var e=t(r);return{state:e,value:e}})},oo=function(t){return function(r){return y1(Yf(t)(r))}};var ah=function(t){return function(r){return function(){return t(r())}}},ih=function(t){return function(){return t}},oh=function(t){return function(r){return function(){return r(t())()}}};function Fe(t){return function(){return{value:t}}}var Ze=function(t){return function(){return t.value}},fh=function(t){return function(r){return function(){var e=t(r.value);return r.value=e.state,e.value}}},Cu=function(t){return function(r){return function(){return r.value=t}}};var T1=function(t,r,e){var n=0,u;return function(a){if(n===2)return u;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+a+")",r,a);return n=1,u=e(),n=2,u}},F1=fh,ja=function(t){return F1(function(r){var e=t(r);return{state:e,value:e}})},Kn={map:ah};var qD={Applicative0:function(){return Ee},Bind1:function(){return Nc}},Nc={bind:oh,Apply0:function(){return HD(0)}},Ee={pure:ih,Apply0:function(){return HD(0)}},HD=T1("applyST","Control.Monad.ST.Internal",function(){return{apply:Gn(qD),Functor0:function(){return Kn}}}),Mv=HD(47),E1=$e(Mv);var M1=F(Ee),zD=function(t){return{append:E1(K(t))}};var lh=function(t){var r=zD(t.Semigroup0());return{mempty:M1(Nt(t)),Semigroup0:function(){return r}}};function Yo(){return[]}var VD=function(t){return function(r){return function(){return r.push.apply(r,t)}}};var Ov=function(t){return function(r){return function(e){return function(n){return function(){return n.splice.apply(n,[t,r].concat(e))}}}}};function O1(t){return function(){return t.slice()}}var dp=O1;var w1=function(){function t(r,e,n,u,a,o){var i,f,m,v,D,A,b;for(i=a+(o-a>>1),i-a>1&&t(r,e,u,n,a,i),o-i>1&&t(r,e,u,n,i,o),f=a,m=i,v=a;f<i&&m<o;)D=u[f],A=u[m],b=e(r(D)(A)),b>0?(n[v++]=A,++m):(n[v++]=D,++f);for(;f<i;)n[v++]=u[f++];for(;m<o;)n[v++]=u[m++]}return function(r){return function(e){return function(n){return function(){return n.length<2||t(r,e,n,n.slice(0),0,n.length),n}}}}}();var co=function(t){return VD([t])};var mh=function(t){return function(r){return function(e){for(var n=r,u=e.length,a=u-1;a>=0;a--)n=t(e[a])(n);return n}}},Dh=function(t){return function(r){return function(e){for(var n=r,u=e.length,a=0;a<u;a++)n=t(n)(e[a]);return n}}};var O={empty:[],Alt0:function(){return FD}},h=function(t){return t.empty};var N=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),qc=function(t){return function(r){return t(r.value0)(r.value1)}};var Ue=function(t){return t.value1};var lo={map:function(t){return function(r){return new N(r.value0,t(r.value1))}}};var pu=function(t){return t.value0};var Zf=function(t){return function(r){return function(e){return t(new N(r,e))}}};var J=function(t){return t};var tn=function(){return J};var QD=tn(),po=function(){return QD};var ie=function(){return QD};var dh=function(){return function(){return function(t){return QD}}};var Xr=function(t){return t.foldr};var rn=function(t){var r=Xr(t);return function(e){return r(gt(e.Alt0()))(h(e))}},_u=function(t){var r=Xr(t);return function(e){var n=gt(e.Alt0()),u=h(e);return function(a){return r(function(o){return n(a(o))})(u)}}},Dn=function(t){var r=rr(t.Apply0()),e=F(t);return function(n){var u=Xr(n);return function(a){return u(function(o){return r(a(o))})(e(void 0))}}},$u=function(t){var r=Dn(t);return function(e){return Gt(r(e))}};var Qr=function(t){return t.foldl};var Me={foldr:function(t){return function(r){return function(e){if(e instanceof w)return r;if(e instanceof $)return t(e.value0)(r);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},foldl:function(t){return function(r){return function(e){if(e instanceof w)return r;if(e instanceof $)return t(r)(e.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},foldMap:function(t){var r=Nt(t);return function(e){return function(n){if(n instanceof w)return r;if(n instanceof $)return e(n.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[e.constructor.name,n.constructor.name])}}}};var gh=function(t){var r=Xr(t);return function(e){var n=K(e.Semigroup0()),u=Nt(e);return function(a){return r(function(o){return function(i){return n(a(o))(i)}})(u)}}},qt={foldr:mh,foldl:Dh,foldMap:function(t){return gh(qt)(t)}};var ge=function(t){return t.foldMap};var hh=function(t){return function(r){for(var e=r.length,n=Array(e),u=0;u<e;u++)n[u]=t(u)(r[u]);return n}};var Tu=function(t){return t.mapWithIndex};var Da={mapWithIndex:hh,Functor0:function(){return ir}};var Ch=function(){function t(u){return[u]}function r(u){return function(a){return[u,a]}}function e(u){return function(a){return function(o){return[u,a,o]}}}function n(u){return function(a){return u.concat(a)}}return function(u){return function(a){return function(o){return function(i){return function(f){function m(v,D){switch(D-v){case 0:return o([]);case 1:return a(t)(i(f[v]));case 2:return u(a(r)(i(f[v])))(i(f[v+1]));case 3:return u(u(a(e)(i(f[v])))(i(f[v+1])))(i(f[v+2]));default:var A=v+Math.floor((D-v)/4)*2;return u(a(n)(m(v,A)))(m(A,D))}}return m(0,f.length)}}}}}}();var MI=Q(Z),en=function(t){return t.traverse};var wh=function(t){var r=en(t);return function(e){return r(e)(MI)}},xi={traverse:function(t){var r=t.Apply0();return Ch(at(r))(g(r.Functor0()))(F(t))},sequence:function(t){return wh(xi)(t)},Functor0:function(){return ir},Foldable1:function(){return qt}};var jI=xn();var XI=K(Ne);var Lp=function(){return wc(N.create)}();var zc=function(){return Cg};var Nh=function(t){return[t]};var Vc=function(){return hg($.create)(w.value)}(),_b=function(t){return Vc(t)(Vn(t)-1|0)};var Uh=function(){return Sg($.create)(w.value)}();var vb=function(){return xg($.create)(w.value)}(),sb=function(t){return function(r){return function(e){return e.length===0?[]:yt(e)(function(n){return jI(vb(n)(e))})(Uh(t(r))(e))}}};var tf=function(t){return function(r){return XI([t])(r)}};var $i=function(t){return t.foldrWithIndex};var Xa=function(t){return t.foldlWithIndex};var vo=function(t){return t.foldMapWithIndex};var rf=function(t){return t.traverseWithIndex};var Ka=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}();var Hv=function(t){var r=h(t);return function(e){return new Ka(e,r)}};var wr=function(){function t(){}return t.value=new t,t}(),Et=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),zv=function(t){return t},oP=function(t){return new Et(t.value0,t.value1)};var fP=function(t){var r=function(e){return function(n){var u=e,a=!1,o;function i(f,m){if(m instanceof Et&&m.value1 instanceof Et&&m.value1.value1 instanceof Et){u=new Et(m,f),n=m.value1.value1.value1;return}var v=function(A){return A instanceof Et&&A.value1 instanceof Et&&A.value1.value1 instanceof wr?new Et(t(A.value0),new Et(t(A.value1.value0),wr.value)):A instanceof Et&&A.value1 instanceof wr?new Et(t(A.value0),wr.value):wr.value},D=function(A){return function(b){var _=A,k=!1,V;function et(Dt,vr){if(Dt instanceof Et&&Dt.value0 instanceof Et&&Dt.value0.value1 instanceof Et&&Dt.value0.value1.value1 instanceof Et){_=Dt.value1,b=new Et(t(Dt.value0.value0),new Et(t(Dt.value0.value1.value0),new Et(t(Dt.value0.value1.value1.value0),vr)));return}return k=!0,vr}for(;!k;)V=et(_,b);return V}};return a=!0,D(f)(v(m))}for(;!a;)o=i(u,n);return o}};return r(wr.value)},Rp={map:fP};var vu={foldr:function(t){return function(r){var e=function(){var u=function(a){return function(o){var i=a,f=!1,m;function v(D,A){if(A instanceof wr)return f=!0,D;if(A instanceof Et){i=new Et(A.value0,D),o=A.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): "+[D.constructor.name,A.constructor.name])}for(;!f;)m=v(i,o);return m}};return u(wr.value)}(),n=Qr(vu)(Gt(t))(r);return function(u){return n(e(u))}}},foldl:function(t){var r=function(e){return function(n){var u=e,a=!1,o;function i(f,m){if(m instanceof wr)return a=!0,f;if(m instanceof Et){u=t(f)(m.value0),n=m.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): "+[m.constructor.name])}for(;!a;)o=i(u,n);return o}};return r},foldMap:function(t){var r=K(t.Semigroup0()),e=Nt(t);return function(n){return Qr(vu)(function(u){var a=r(u);return function(o){return a(n(o))}})(e)}}};var cP=Xr(vu);var Vv={append:function(t){return function(r){return cP(Et.create)(r)(t)}}},qh=K(Vv);var bb={append:function(t){return function(r){return new Ka(t.value0,qh(t.value1)(oP(r)))}}};var Hh={alt:qh,Functor0:function(){return Rp}},Ab=function(){return{empty:wr.value,Alt0:function(){return Hh}}}();var kb=function(){var t=function(r){return function(e){var n=r,u=!1,a;function o(i,f){if(f instanceof wr)return u=!0,i;if(f instanceof Et){n=new Et(f.value0,i),e=f.value1;return}throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): "+[i.constructor.name,f.constructor.name])}for(;!u;)a=o(n,e);return a}};return t(wr.value)}();var Xh=function(t){return t()};var Qh=function(t){throw new Error(t)};var Kh=function(){return Qh};var MP=Kh(),OP=Xh,Fu=function(t){return OP(function(){return MP(t)})};var Lt=function(){function t(){}return t.value=new t,t}(),er=function(){function t(r,e,n,u){this.value0=r,this.value1=e,this.value2=n,this.value3=u}return t.create=function(r){return function(e){return function(n){return function(u){return new t(r,e,n,u)}}}},t}(),Pr=function(){function t(r,e,n,u,a,o,i){this.value0=r,this.value1=e,this.value2=n,this.value3=u,this.value4=a,this.value5=o,this.value6=i}return t.create=function(r){return function(e){return function(n){return function(u){return function(a){return function(o){return function(i){return new t(r,e,n,u,a,o,i)}}}}}}},t}(),uf=function(){function t(r,e,n){this.value0=r,this.value1=e,this.value2=n}return t.create=function(r){return function(e){return function(n){return new t(r,e,n)}}},t}(),mo=function(){function t(r,e,n){this.value0=r,this.value1=e,this.value2=n}return t.create=function(r){return function(e){return function(n){return new t(r,e,n)}}},t}(),af=function(){function t(r,e,n,u,a,o){this.value0=r,this.value1=e,this.value2=n,this.value3=u,this.value4=a,this.value5=o}return t.create=function(r){return function(e){return function(n){return function(u){return function(a){return function(o){return new t(r,e,n,u,a,o)}}}}}},t}(),Ei=function(){function t(r,e,n,u,a,o){this.value0=r,this.value1=e,this.value2=n,this.value3=u,this.value4=a,this.value5=o}return t.create=function(r){return function(e){return function(n){return function(u){return function(a){return function(o){return new t(r,e,n,u,a,o)}}}}}},t}(),of=function(){function t(r,e,n,u,a,o){this.value0=r,this.value1=e,this.value2=n,this.value3=u,this.value4=a,this.value5=o}return t.create=function(r){return function(e){return function(n){return function(u){return function(a){return function(o){return new t(r,e,n,u,a,o)}}}}}},t}(),Jv=function(){function t(r,e,n,u){this.value0=r,this.value1=e,this.value2=n,this.value3=u}return t.create=function(r){return function(e){return function(n){return function(u){return new t(r,e,n,u)}}}},t}();var tS=function(t){return function(r){return new er(Lt.value,t,r,Lt.value)}};var Xv=function(t){var r=Pt(t);return function(e){var n=function(u){var a=!1,o;function i(f){if(f instanceof Lt)return a=!0,w.value;if(f instanceof er){var m=r(e)(f.value1);if(m instanceof Xt)return a=!0,new $(f.value2);if(m instanceof It){u=f.value0;return}u=f.value3;return}if(f instanceof Pr){var v=r(e)(f.value1);if(v instanceof Xt)return a=!0,new $(f.value2);var D=r(e)(f.value4);if(D instanceof Xt)return a=!0,new $(f.value5);if(v instanceof It){u=f.value0;return}if(D instanceof Jt){u=f.value6;return}u=f.value3;return}throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): "+[f.constructor.name])}for(;!a;)o=i(u);return o};return n}};var rS=function(t){return t instanceof Lt};var eS=function(t){return function(r){return function(e){var n=t,u=r,a=!1,o;function i(f,m,v){if(m instanceof wr)return a=!0,v;if(m instanceof Et){if(m.value0 instanceof uf){n=f,u=m.value1,e=new er(v,m.value0.value0,m.value0.value1,m.value0.value2);return}if(m.value0 instanceof mo){n=f,u=m.value1,e=new er(m.value0.value0,m.value0.value1,m.value0.value2,v);return}if(m.value0 instanceof af){n=f,u=m.value1,e=new Pr(v,m.value0.value0,m.value0.value1,m.value0.value2,m.value0.value3,m.value0.value4,m.value0.value5);return}if(m.value0 instanceof Ei){n=f,u=m.value1,e=new Pr(m.value0.value0,m.value0.value1,m.value0.value2,v,m.value0.value3,m.value0.value4,m.value0.value5);return}if(m.value0 instanceof of){n=f,u=m.value1,e=new Pr(m.value0.value0,m.value0.value1,m.value0.value2,m.value0.value3,m.value0.value4,m.value0.value5,v);return}throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): "+[m.value0.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): "+[m.constructor.name,v.constructor.name])}for(;!a;)o=i(n,u,e);return o}}},Up=function(t){var r=eS(t),e=Pt(t);return function(n){return function(u){var a=function(i){return function(f){var m=i,v=!1,D;function A(b,_){if(b instanceof wr)return v=!0,new er(_.value0,_.value1,_.value2,_.value3);if(b instanceof Et){if(b.value0 instanceof uf)return v=!0,r(b.value1)(new Pr(_.value0,_.value1,_.value2,_.value3,b.value0.value0,b.value0.value1,b.value0.value2));if(b.value0 instanceof mo)return v=!0,r(b.value1)(new Pr(b.value0.value0,b.value0.value1,b.value0.value2,_.value0,_.value1,_.value2,_.value3));if(b.value0 instanceof af){m=b.value1,f=new Jv(new er(_.value0,_.value1,_.value2,_.value3),b.value0.value0,b.value0.value1,new er(b.value0.value2,b.value0.value3,b.value0.value4,b.value0.value5));return}if(b.value0 instanceof Ei){m=b.value1,f=new Jv(new er(b.value0.value0,b.value0.value1,b.value0.value2,_.value0),_.value1,_.value2,new er(_.value3,b.value0.value3,b.value0.value4,b.value0.value5));return}if(b.value0 instanceof of){m=b.value1,f=new Jv(new er(b.value0.value0,b.value0.value1,b.value0.value2,b.value0.value3),b.value0.value4,b.value0.value5,new er(_.value0,_.value1,_.value2,_.value3));return}throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): "+[b.value0.constructor.name,_.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): "+[b.constructor.name,_.constructor.name])}for(;!v;)D=A(m,f);return D}},o=function(i){return function(f){var m=i,v=!1,D;function A(b,_){if(_ instanceof Lt)return v=!0,a(b)(new Jv(Lt.value,n,u,Lt.value));if(_ instanceof er){var k=e(n)(_.value1);if(k instanceof Xt)return v=!0,r(b)(new er(_.value0,n,u,_.value3));if(k instanceof It){m=new Et(new uf(_.value1,_.value2,_.value3),b),f=_.value0;return}m=new Et(new mo(_.value0,_.value1,_.value2),b),f=_.value3;return}if(_ instanceof Pr){var k=e(n)(_.value1);if(k instanceof Xt)return v=!0,r(b)(new Pr(_.value0,n,u,_.value3,_.value4,_.value5,_.value6));var V=e(n)(_.value4);if(V instanceof Xt)return v=!0,r(b)(new Pr(_.value0,_.value1,_.value2,_.value3,n,u,_.value6));if(k instanceof It){m=new Et(new af(_.value1,_.value2,_.value3,_.value4,_.value5,_.value6),b),f=_.value0;return}if(k instanceof Jt&&V instanceof It){m=new Et(new Ei(_.value0,_.value1,_.value2,_.value4,_.value5,_.value6),b),f=_.value3;return}m=new Et(new of(_.value0,_.value1,_.value2,_.value3,_.value4,_.value5),b),f=_.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): "+[b.constructor.name,_.constructor.name])}for(;!v;)D=A(m,f);return D}};return o(wr.value)}}},RP=function(t){var r=eS(t),e=Pt(t);return function(n){var u=function(f){return function(m){var v=f,D=!1,A;function b(_,k){if(_ instanceof wr)return D=!0,k;if(_ instanceof Et){if(_.value0 instanceof uf&&_.value0.value2 instanceof Lt&&k instanceof Lt)return D=!0,r(_.value1)(new er(Lt.value,_.value0.value0,_.value0.value1,Lt.value));if(_.value0 instanceof mo&&_.value0.value0 instanceof Lt&&k instanceof Lt)return D=!0,r(_.value1)(new er(Lt.value,_.value0.value1,_.value0.value2,Lt.value));if(_.value0 instanceof uf&&_.value0.value2 instanceof er){v=_.value1,m=new Pr(k,_.value0.value0,_.value0.value1,_.value0.value2.value0,_.value0.value2.value1,_.value0.value2.value2,_.value0.value2.value3);return}if(_.value0 instanceof mo&&_.value0.value0 instanceof er){v=_.value1,m=new Pr(_.value0.value0.value0,_.value0.value0.value1,_.value0.value0.value2,_.value0.value0.value3,_.value0.value1,_.value0.value2,k);return}return _.value0 instanceof uf&&_.value0.value2 instanceof Pr?(D=!0,r(_.value1)(new er(new er(k,_.value0.value0,_.value0.value1,_.value0.value2.value0),_.value0.value2.value1,_.value0.value2.value2,new er(_.value0.value2.value3,_.value0.value2.value4,_.value0.value2.value5,_.value0.value2.value6)))):_.value0 instanceof mo&&_.value0.value0 instanceof Pr?(D=!0,r(_.value1)(new er(new er(_.value0.value0.value0,_.value0.value0.value1,_.value0.value0.value2,_.value0.value0.value3),_.value0.value0.value4,_.value0.value0.value5,new er(_.value0.value0.value6,_.value0.value1,_.value0.value2,k)))):_.value0 instanceof af&&_.value0.value2 instanceof Lt&&_.value0.value5 instanceof Lt&&k instanceof Lt?(D=!0,r(_.value1)(new Pr(Lt.value,_.value0.value0,_.value0.value1,Lt.value,_.value0.value3,_.value0.value4,Lt.value))):_.value0 instanceof Ei&&_.value0.value0 instanceof Lt&&_.value0.value5 instanceof Lt&&k instanceof Lt?(D=!0,r(_.value1)(new Pr(Lt.value,_.value0.value1,_.value0.value2,Lt.value,_.value0.value3,_.value0.value4,Lt.value))):_.value0 instanceof of&&_.value0.value0 instanceof Lt&&_.value0.value3 instanceof Lt&&k instanceof Lt?(D=!0,r(_.value1)(new Pr(Lt.value,_.value0.value1,_.value0.value2,Lt.value,_.value0.value4,_.value0.value5,Lt.value))):_.value0 instanceof af&&_.value0.value2 instanceof er?(D=!0,r(_.value1)(new er(new Pr(k,_.value0.value0,_.value0.value1,_.value0.value2.value0,_.value0.value2.value1,_.value0.value2.value2,_.value0.value2.value3),_.value0.value3,_.value0.value4,_.value0.value5))):_.value0 instanceof Ei&&_.value0.value0 instanceof er?(D=!0,r(_.value1)(new er(new Pr(_.value0.value0.value0,_.value0.value0.value1,_.value0.value0.value2,_.value0.value0.value3,_.value0.value1,_.value0.value2,k),_.value0.value3,_.value0.value4,_.value0.value5))):_.value0 instanceof Ei&&_.value0.value5 instanceof er?(D=!0,r(_.value1)(new er(_.value0.value0,_.value0.value1,_.value0.value2,new Pr(k,_.value0.value3,_.value0.value4,_.value0.value5.value0,_.value0.value5.value1,_.value0.value5.value2,_.value0.value5.value3)))):_.value0 instanceof of&&_.value0.value3 instanceof er?(D=!0,r(_.value1)(new er(_.value0.value0,_.value0.value1,_.value0.value2,new Pr(_.value0.value3.value0,_.value0.value3.value1,_.value0.value3.value2,_.value0.value3.value3,_.value0.value4,_.value0.value5,k)))):_.value0 instanceof af&&_.value0.value2 instanceof Pr?(D=!0,r(_.value1)(new Pr(new er(k,_.value0.value0,_.value0.value1,_.value0.value2.value0),_.value0.value2.value1,_.value0.value2.value2,new er(_.value0.value2.value3,_.value0.value2.value4,_.value0.value2.value5,_.value0.value2.value6),_.value0.value3,_.value0.value4,_.value0.value5))):_.value0 instanceof Ei&&_.value0.value0 instanceof Pr?(D=!0,r(_.value1)(new Pr(new er(_.value0.value0.value0,_.value0.value0.value1,_.value0.value0.value2,_.value0.value0.value3),_.value0.value0.value4,_.value0.value0.value5,new er(_.value0.value0.value6,_.value0.value1,_.value0.value2,k),_.value0.value3,_.value0.value4,_.value0.value5))):_.value0 instanceof Ei&&_.value0.value5 instanceof Pr?(D=!0,r(_.value1)(new Pr(_.value0.value0,_.value0.value1,_.value0.value2,new er(k,_.value0.value3,_.value0.value4,_.value0.value5.value0),_.value0.value5.value1,_.value0.value5.value2,new er(_.value0.value5.value3,_.value0.value5.value4,_.value0.value5.value5,_.value0.value5.value6)))):_.value0 instanceof of&&_.value0.value3 instanceof Pr?(D=!0,r(_.value1)(new Pr(_.value0.value0,_.value0.value1,_.value0.value2,new er(_.value0.value3.value0,_.value0.value3.value1,_.value0.value3.value2,_.value0.value3.value3),_.value0.value3.value4,_.value0.value3.value5,new er(_.value0.value3.value6,_.value0.value4,_.value0.value5,k)))):(D=!0,Fu("The impossible happened in partial function `up`."))}throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): "+[_.constructor.name])}for(;!D;)A=b(v,m);return A}},a=function(f){return function(m){var v=f,D=!1,A;function b(_,k){if(k instanceof er&&k.value0 instanceof Lt&&k.value3 instanceof Lt)return D=!0,u(_)(Lt.value);if(k instanceof er){v=new Et(new mo(k.value0,k.value1,k.value2),_),m=k.value3;return}if(k instanceof Pr&&k.value0 instanceof Lt&&k.value3 instanceof Lt&&k.value6 instanceof Lt)return D=!0,u(new Et(new mo(Lt.value,k.value1,k.value2),_))(Lt.value);if(k instanceof Pr){v=new Et(new of(k.value0,k.value1,k.value2,k.value3,k.value4,k.value5),_),m=k.value6;return}return D=!0,Fu("The impossible happened in partial function `removeMaxNode`.")}for(;!D;)A=b(v,m);return A}},o=function(f){var m=!1,v;function D(A){if(A instanceof er&&A.value3 instanceof Lt)return m=!0,{key:A.value1,value:A.value2};if(A instanceof er){f=A.value3;return}if(A instanceof Pr&&A.value6 instanceof Lt)return m=!0,{key:A.value4,value:A.value5};if(A instanceof Pr){f=A.value6;return}return m=!0,Fu("The impossible happened in partial function `maxNode`.")}for(;!m;)v=D(f);return v},i=function(f){return function(m){var v=f,D=!1,A;function b(_,k){if(k instanceof Lt)return D=!0,w.value;if(k instanceof er){var V=e(n)(k.value1);if(k.value3 instanceof Lt&&V instanceof Xt)return D=!0,new $(new N(k.value2,u(_)(Lt.value)));if(V instanceof Xt){var et=o(k.value0);return D=!0,new $(new N(k.value2,a(new Et(new uf(et.key,et.value,k.value3),_))(k.value0)))}if(V instanceof It){v=new Et(new uf(k.value1,k.value2,k.value3),_),m=k.value0;return}v=new Et(new mo(k.value0,k.value1,k.value2),_),m=k.value3;return}if(k instanceof Pr){var Dt=function(){return k.value0 instanceof Lt&&k.value3 instanceof Lt&&k.value6 instanceof Lt}(),V=e(n)(k.value4),vr=e(n)(k.value1);if(Dt&&vr instanceof Xt)return D=!0,new $(new N(k.value2,r(_)(new er(Lt.value,k.value4,k.value5,Lt.value))));if(Dt&&V instanceof Xt)return D=!0,new $(new N(k.value5,r(_)(new er(Lt.value,k.value1,k.value2,Lt.value))));if(vr instanceof Xt){var et=o(k.value0);return D=!0,new $(new N(k.value2,a(new Et(new af(et.key,et.value,k.value3,k.value4,k.value5,k.value6),_))(k.value0)))}if(V instanceof Xt){var et=o(k.value3);return D=!0,new $(new N(k.value5,a(new Et(new Ei(k.value0,k.value1,k.value2,et.key,et.value,k.value6),_))(k.value3)))}if(vr instanceof It){v=new Et(new af(k.value1,k.value2,k.value3,k.value4,k.value5,k.value6),_),m=k.value0;return}if(vr instanceof Jt&&V instanceof It){v=new Et(new Ei(k.value0,k.value1,k.value2,k.value4,k.value5,k.value6),_),m=k.value3;return}v=new Et(new of(k.value0,k.value1,k.value2,k.value3,k.value4,k.value5),_),m=k.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): "+[k.constructor.name])}for(;!D;)A=b(v,m);return A}};return i(wr.value)}},Un={foldr:function(t){return function(r){return function(e){if(e instanceof Lt)return r;if(e instanceof er)return Xr(Un)(t)(t(e.value2)(Xr(Un)(t)(r)(e.value3)))(e.value0);if(e instanceof Pr)return Xr(Un)(t)(t(e.value2)(Xr(Un)(t)(t(e.value5)(Xr(Un)(t)(r)(e.value6)))(e.value3)))(e.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): "+[e.constructor.name])}}},foldl:function(t){return function(r){return function(e){if(e instanceof Lt)return r;if(e instanceof er)return Qr(Un)(t)(t(Qr(Un)(t)(r)(e.value0))(e.value2))(e.value3);if(e instanceof Pr)return Qr(Un)(t)(t(Qr(Un)(t)(t(Qr(Un)(t)(r)(e.value0))(e.value2))(e.value3))(e.value5))(e.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): "+[e.constructor.name])}}},foldMap:function(t){var r=Nt(t),e=K(t.Semigroup0());return function(n){return function(u){if(u instanceof Lt)return r;if(u instanceof er)return e(ge(Un)(t)(n)(u.value0))(e(n(u.value2))(ge(Un)(t)(n)(u.value3)));if(u instanceof Pr)return e(ge(Un)(t)(n)(u.value0))(e(n(u.value2))(e(ge(Un)(t)(n)(u.value3))(e(n(u.value5))(ge(Un)(t)(n)(u.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): "+[u.constructor.name])}}}},Bn={foldrWithIndex:function(t){return function(r){return function(e){if(e instanceof Lt)return r;if(e instanceof er)return $i(Bn)(t)(t(e.value1)(e.value2)($i(Bn)(t)(r)(e.value3)))(e.value0);if(e instanceof Pr)return $i(Bn)(t)(t(e.value1)(e.value2)($i(Bn)(t)(t(e.value4)(e.value5)($i(Bn)(t)(r)(e.value6)))(e.value3)))(e.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 147, column 26 - line 150, column 120): "+[e.constructor.name])}}},foldlWithIndex:function(t){return function(r){return function(e){if(e instanceof Lt)return r;if(e instanceof er)return Xa(Bn)(t)(t(e.value1)(Xa(Bn)(t)(r)(e.value0))(e.value2))(e.value3);if(e instanceof Pr)return Xa(Bn)(t)(t(e.value4)(Xa(Bn)(t)(t(e.value1)(Xa(Bn)(t)(r)(e.value0))(e.value2))(e.value3))(e.value5))(e.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 151, column 26 - line 154, column 120): "+[e.constructor.name])}}},foldMapWithIndex:function(t){var r=Nt(t),e=K(t.Semigroup0());return function(n){return function(u){if(u instanceof Lt)return r;if(u instanceof er)return e(vo(Bn)(t)(n)(u.value0))(e(n(u.value1)(u.value2))(vo(Bn)(t)(n)(u.value3)));if(u instanceof Pr)return e(vo(Bn)(t)(n)(u.value0))(e(n(u.value1)(u.value2))(e(vo(Bn)(t)(n)(u.value3))(e(n(u.value4)(u.value5))(vo(Bn)(t)(n)(u.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 155, column 26 - line 158, column 128): "+[u.constructor.name])}}},Foldable0:function(){return Un}},NP=$i(Bn),UP=Xa(Bn),nS=function(){return NP(function(t){return function(r){return function(e){return new Et(t,e)}}})(wr.value)}();var uc=function(){return Lt.value}();var Sb=function(t){var r=RP(t);return function(e){return function(n){return yt(n)(Ue)(r(e)(n))}}};var ac=function(t){var r=Xv(t),e=Sb(t),n=Up(t);return function(u){return function(a){return function(o){var i=u(r(a)(o));if(i instanceof w)return e(a)(o);if(i instanceof $)return n(a)(i.value0)(o);throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): "+[i.constructor.name])}}}};var BP=function(t){var r=ac(t);return function(e){return function(n){return function(u){var a=function(o){return function(i){return function(f){return r(function(){var m=yt(f)(e(f));return function(v){return $.create(m(v))}}())(o)(i)}}};return UP(a)(u)(n)}}}};var uS=function(t){return BP(t)(M)};var Qv=function(t){return t.partitionMap};var Do=function(t){return t.filterMap};var qp=function(t){return t.filter};var fS=t=>{for(var r=0,e=t.length;r<e;r++)t[r]()},Kv=(t,r)=>{for(var e=0,n=t.length;e<n;e++)r(t[e])},xb=(t,r)=>{for(let e in t)r(t[e])},Cb=()=>({}),$b=(t,r,e)=>{e[t]=r},Tb=(t,r)=>{delete r[t]};var cS=J;var ba={liftST:cS,Monad0:function(){return ao}},Aa=function(t){return t.liftST};var XP=ge(vu),QP=Qr(vu),KP=Xr(vu);var YP=function(t){var r=uS(t);return function(e){return function(n){return r(e)(n)}}};var Fb=function(t){return nS(t)};var _S=function(t){return tS(t)(void 0)};var Eb=function(t){return{append:YP(t)}};var vS=function(t){return rS(t)},sS=function(t){var r=Up(t);return function(e){return function(n){return r(e)(void 0)(n)}}};var mS={foldMap:function(t){var r=XP(t);return function(e){var n=r(e);return function(u){return n(Fb(u))}}},foldl:function(t){return function(r){var e=QP(t)(r);return function(n){return e(Fb(n))}}},foldr:function(t){return function(r){var e=KP(t)(r);return function(n){return e(Fb(n))}}}};var Mb=uc;var DS=function(t){var r=Eb(t);return{mempty:Mb,Semigroup0:function(){return r}}};var Yv=function(t){var r=Sb(t);return function(e){return function(n){return r(e)(n)}}};function bS(t){return function(r){return function(){return setTimeout(r,t)}}}function AS(t){return function(){clearTimeout(t)}}var tL=Pt(Te);var Zv=bS;var rL={eq:function(t){return function(r){return t===r}}},ts={compare:function(t){return function(r){return tL(t)(r)}},Eq0:function(){return rL}};var zp=AS;var bo=function(r){return function(e){return r(e)()}};var jc=function(r){return function(e){return function(){return r(e)}}};var AL=function(t){var r=K(BD(t));return{append:function(e){return function(n){return bo(function(u){return r(jc(e)(u))(jc(n)(u))})}}}};var gS=function(t){var r=Nt(io(t)),e=AL(t.Semigroup0());return{mempty:bo(function(n){return r}),Semigroup0:function(){return e}}};var kL=Q(Z),dL=g(lo),yL=F(no),Ao=function(t){return t.sampleOnRight};var ka=function(t){return t.keepLatest};var Xc=function(t){return t.fix},da=function(t){var r=Xc(t),e=Ao(t),n=t.Alternative0(),u=gt(n.Plus1().Alt0()),a=F(n.Applicative0()),o=g(t.Filterable1().Functor1());return function(i){return function(f){return function(m){return r(function(v){return e(u(v)(a(f)))(o(Gt(i))(m))})}}}};var Qc=function(t){var r=Do(t.Filterable1()),e=da(t);return function(n){return function(u){return function(a){return r(Ue)(e(function(o){return function(i){return dL(yL)(n(o.value0)(i))}})(new N(u,w.value))(a))}}}},rs=function(t){var r=Do(t.Filterable1()),e=da(t);return function(n){var u=function(a){return function(o){if(a instanceof w)return new $({now:o,last:w.value});if(a instanceof $)return new $({now:o,last:new $(a.value0.now)});throw new Error("Failed pattern match at FRP.Event.Class (line 83, column 3 - line 83, column 50): "+[a.constructor.name,o.constructor.name])}};return r(kL)(e(u)(w.value)(n))}};function wb(t){return function(r){return t===r}}var Ib=wb;var xS=function(t,r,e){var n=0,u;return function(a){if(n===2)return u;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+a+")",r,a);return n=1,u=e(),n=2,u}},CS=$u(Bt),ns=CS(Me),fc=F(Bt),Za=Aa(ba),SL=io(uo),Jp=Ir(L),xL=K(Ne),$S=Nt(gS(uo)),CL=Nt(DS(ts)),$L=Yv(ts),TL=K(Eb(ts)),FL=CS(mS),EL=at(or),ML=g(L);var OL=function(t){return function(r){return function(e,n){var u=Or(w.value)(),a=t(e,function(i){return te(new $(i))(u)()}),o=r(e,function(i){var f=Ur(u)();return ns(f)(function(m){return function(){return n(i(m))}})()});return function(){return a(),o()}}}},wL=function(t){return function(r){return function(e,n){var u=Or(w.value)(),a=t(e,function(i){var f=Ur(u)();return ns(f)(function(m){return function(){return n(m(i))}})()}),o=r(e,function(i){return te(new $(i))(u)()});return function(){return a(),o()}}}},cf=function(t){var r=ge(t)(SL);return function(e){return function(n,u){var a=Za(Yo)();return r(function(o){return function(){var f=o(n,u);return Jp(Za(co(f)(a)))()}})(e)(),function(){var i=Za(dp(a))();return fS(i)}}}},SS=function(t){var r=ac(t),e=Xv(t);return function(){var u=Or(uc)();return{event:function(a){return function(o,i){return Jp(Yf(r(function(f){if(f instanceof w)return new $([i]);if(f instanceof $)return new $(xL(f.value0)([i]));throw new Error("Failed pattern match at FRP.Event (line 568, column 17 - line 570, column 51): "+[f.constructor.name])})(a))(u))(),Jp(Yf(r(function(f){if(f instanceof w)return w.value;if(f instanceof $)return new $(sb(Ib)(i)(f.value0));throw new Error("Failed pattern match at FRP.Event (line 577, column 17 - line 579, column 65): "+[f.constructor.name])})(a))(u))}},push:function(a){var o=Ur(u)(),i=e(a.address)(o);if(i instanceof w)return void 0;if(i instanceof $)return Kv(i.value0,function(f){return f(a.payload)});throw new Error("Failed pattern match at FRP.Event (line 586, column 9 - line 588, column 95): "+[i.constructor.name])}}}},IL=function(t){return function(r,e){var n=Or(fc(void 0))(),u=t(r,function(a){var o=Ur(n)();o();var i=a(r,e);return te(i)(n)()});return function(){var o=Ur(n)();return o(),u()}}},ft={map:function(t){return function(r){return function(e,n){return r(e,function(u){return n(t(u))})}}}},PL=g(ft),Gp=function(t){return function(r){return function(e,n){return r(e,function(u){var a=t(u);if(a instanceof $)return n(a.value0);if(a instanceof w)return void 0;throw new Error("Failed pattern match at FRP.Event (line 225, column 31 - line 227, column 35): "+[a.constructor.name])})}}},Pb=function(t){return Gp(function(r){var e=t(r);if(e)return new $(r);if(!e)return w.value;throw new Error("Failed pattern match at FRP.Event (line 141, column 13 - line 143, column 25): "+[e.constructor.name])})},es=function(){var r=Cb(),e=Or(0)();return{event:function(n,u){var a=Or(u)(),o=Ur(e)();return $b(o,a,r),oo(function(i){return i+1|0})(e)(),function(){return te($S)(a)(),Tb(o,r),void 0}},push:function(n){return xb(r,function(u){var a=Ur(u)();return a(n)})}}},LL=function(t){return function(r,e){var n=es(),u=t(n.event),a=n.event(r,e),o=u(r,n.push);return function(){return o(),a()}}},TS={compact:Gp(Q(Z)),separate:function(t){return{left:Gp(function(r){if(r instanceof St)return new $(r.value0);if(r instanceof xt)return w.value;throw new Error("Failed pattern match at FRP.Event (line 124, column 13 - line 126, column 33): "+[r.constructor.name])})(t),right:Gp(function(r){if(r instanceof xt)return new $(r.value0);if(r instanceof St)return w.value;throw new Error("Failed pattern match at FRP.Event (line 131, column 13 - line 133, column 32): "+[r.constructor.name])})(t)}}},ya={filter:Pb,filterMap:Gp,partition:function(t){return function(r){return{yes:Pb(t)(r),no:Pb(function(e){return!t(e)})(r)}}},partitionMap:function(t){return function(r){return{left:Do(ya)(function(){var e=jn($.create)(M(w.value));return function(n){return e(t(n))}}())(r),right:Do(ya)(function(e){return Fv(t(e))})(r)}}},Compactable0:function(){return TS},Functor1:function(){return ft}},RL=function(t){return function(r){return function(e,n){var u=Or(w.value)(),a=Za(Yo)(),o=Or(w.value)(),i=Za(Yo)(),f=Or(!0)(),m=t(e,function(b){var _=Ur(f)();if(_)return Jp(Za(co(b)(a)))();te(new $(b))(u)();var k=Ur(o)();return ns(k)(function(V){return function(){return n(V(b))}})()}),v=r(e,function(b){var _=Ur(f)();if(_)return Jp(Za(co(b)(i)))();te(new $(b))(o)();var k=Ur(u)();return ns(k)(function(V){return function(){return n(b(V))}})()});te(!1)(f)();var D=Za(dp(a))(),A=Za(dp(i))();return function(){return D.length===0?te(_b(A))(o)():Kv(D,function(b){return te(new $(b))(u)(),Kv(A,function(_){return te(new $(_))(o)(),n(_(b))})})}(),Za(Ov(0)(Vn(D))([])(a))(),Za(Ov(0)(Vn(A))([])(i))(),function(){return m(),v()}}}},we=function(t){return function(r){return r}(Rb(345).subscribe)(t)},Rb=xS("backdoor","FRP.Event",function(){var t=function(){var e=Cb(),n=Or(0)();return{event:function(u,a){var o=Or(a)(),i=Ur(n)();return $b(i,o,e),oo(function(f){return f+1|0})(n)(),function(){return te($S)(o)(),Tb(i,e),void 0}},push:function(u){return function(){return xb(e,function(a){var o=Ur(a)();return o(u)})}}}};return{createO:es,makeEvent:function(){var r=function(e){return function(n,u){return n?fc(void 0):e(function(a){return function(){return u(a)}})()}};return r}(),makeEventO:function(){var r=function(e){return function(n,u){return n?fc(void 0):e(u)}};return r}(),makePureEvent:function(){var r=function(e){return function(n,u){return e(function(a){return function(){return u(a)}})()}};return r}(),makeLemmingEvent:function(){var r=function(e){return function(n,u){var a=function(o){return function(i){return function(){return o(n,bo(i))}}};return e(a)(function(o){return function(){return u(o)}})()}};return r}(),makeLemmingEventO:function(){var r=function(e){return function(n,u){var a=function(o,i){return o(n,i)};return e(a,u)}};return r}(),create:t,createPure:t,createPureO:es,subscribe:function(){var r=function(e){return function(n){return function(){return e(!1,bo(n))}}};return r}(),subscribeO:function(){var r=function(e,n){return e(!1,n)};return r}(),subscribePureO:function(){var r=function(e,n){return e(!0,n)};return r}(),subscribePure:function(){var r=function(){var e=function(n){return function(u){return function(){return n(!0,bo(u))}}};return e}();return r}(),bus:function(){var r=function(e){return function(n,u){var a=Lb(819)();return u(e(a.push)(a.event)),fc(void 0)}};return r}(),memoize:function(){var r=function(e){return function(n){return function(u,a){var o=es();return a(n(o.event)),e(u,o.push)}}};return r}(),hot:function(){var r=function(e){return function(){var u=Lb(837)(),a=we(e)(u.push)();return{event:u.event,unsubscribe:a}}};return r}(),mailbox:function(){var r=function(e){return function(){var u=SS(e)();return{event:u.event,push:function(a){return function(){return u.push(a)}}}}};return r}(),mailboxed:function(){var r=function(e){var n=SS(e);return function(u){return function(a){return function(o,i){var f=n();return i(a(f.event)),u(o,f.push)}}}};return r}(),delay:function(){var r=function(e){return function(n){return function(u,a){var o=Or(CL)(),i=n(u,function(f){var m=Or(w.value)(),v=Zv(e)(function(){a(f);var A=Ur(m)();return yt(fc(void 0))(function(b){return oo($L(b))(o)})(A)()})();return te(new $(v))(m)(),oo(TL(_S(v)))(o)()});return function(){var m=Ur(o)();return FL(m)(zp)(),i()}}}};return r}()}}),Lb=xS("create","FRP.Event",function(){return function(){return void 0,function(r){return r}(Rb(461).create)()}}),Kc=Rb(678),FS=Lb(458),Nb=function(t){return function(r){return r}(Kc.bus)(t)};var ga=function(t){return function(r){return r}(Kc.delay)(t)};var Cn=function(t){return function(r){return r}(Kc.makeEvent)(t)};var Ub=function(t){return function(r){return r}(Kc.makeLemmingEvent)(t)},mr=function(t){return function(r){return r}(Kc.makeLemmingEventO)(t)};var ti=function(t){return function(r){return r}(Kc.memoize)(t)};var pn={apply:function(t){return function(r){return RL(t)(PL(Jf)(r))}},Functor0:function(){return ft}},NL=$e(pn),UL=function(t){return{append:NL(K(t))}},j={pure:function(t){return function(r,e){return e(t),fc(void 0)}},Apply0:function(){return pn}},BL=F(j);var ES=function(t){var r=UL(t.Semigroup0());return{mempty:BL(Nt(t)),Semigroup0:function(){return r}}};var Wt={alt:function(t){return function(r){return function(e,n){return EL(ML(function(u){return function(a){return function(){return u(),a()}}})(function(){return t(e,n)}))(function(){return r(e,n)})()}}},Functor0:function(){return ft}};var Dr={empty:function(t,r){return fc(void 0)},Alt0:function(){return Wt}},WL={Applicative0:function(){return j},Plus1:function(){return Dr}},me={keepLatest:IL,sampleOnRight:OL,sampleOnLeft:wL,fix:LL,Alternative0:function(){return WL},Filterable1:function(){return ya}};var qL=function(t){return t},jp=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Xp=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),us=function(){function t(){}return t.value=new t,t}(),HL=function(t){return t};var as=tn(),c=HL;var s=function(){return jp.create}();var G=function(){return Xp.create}(),Vr=function(){var t=g(Qe)(g(L)(M(!0)));return function(r){return qL(t(r))}}(),st=function(t){return t.attr};function MS(t){return()=>t.slice()}function OS(t){return r=>e=>()=>{e[t]=r}}function wS(t){return()=>t.slice()}function Qp(t,r){var e={};for(var n in r)({}).hasOwnProperty.call(r,n)&&(e[n]=r[n]);for(var u in t)({}).hasOwnProperty.call(t,u)&&(e[u]=t[u]);return e}var PS=function(t){var r=fe(t);return function(){return function(){return function(e){return function(n){return function(u){return qa(r(e))(n)(u)}}}}}};var LS=function(){return function(){return function(t){return function(r){return Qp(t,r)}}}},Kp=function(t){var r=fe(t);return function(){return function(){return function(e){return function(n){return function(u){return qa(r(e))(n)(u)}}}}}},ko=function(t){var r=fe(t);return function(){return function(e){return function(n){return Au(r(e))(n)}}}};var RS=F(Ee);var Yn={vb:function(t){return RS(new N({},{}))}},is=function(t){return t.vb},XL={vbus:function(){var t=function(){return function(n){var u=is(n);return function(a){return function(o){return Ub(function(i){return function(f){return function(){var v=u(d.value)();return f(o(v.value0)(v.value1))(),RS(void 0)}}})}}}},r=t(),e=function(){return function(n){return r(n)}};return e}()},Wb=function(){return function(t){return function(r){return function(e){return e()(t)}(XL.vbus)(r)}}},Mi=function(t){var r=Kp(t)()();return function(){return function(){return function(){return function(e){var n=is(e);return function(u){var a=is(u);return function(){return function(){return{vb:function(o){return function(){var f=a(d.value)(),m=n(d.value)();return new N(r(d.value)(m.value0)(f.value0),r(d.value)(m.value1)(f.value1))}}}}}}}}}}},De=function(t){var r=Kp(t)()();return function(){return function(){return function(e){var n=is(e);return function(){return function(){return{vb:function(u){return function(){var o=n(d.value)(),i=FS();return new N(r(d.value)(i.push)(o.value0),r(d.value)(i.event)(o.value1))}}}}}}}}};var Eu=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),lf=function(){function t(){}return t.value=new t,t}();var Yc=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Zc=function(){function t(){}return t.value=new t,t}(),qb=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Hb=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Yp=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),ri=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),_r=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var NS=function(t){return t};var go={eq:function(t){return function(r){return t instanceof Eu&&r instanceof Eu?t.value0===r.value0:t instanceof lf&&r instanceof lf}}};var ze=function(t){return new Yp(t)},tl=function(t){return new ri(t)},Zp=function(t){return new Hb(t)};var US=t=>r=>r[t];var KL=function(){function t(o){this.fn=o}var r={},e=function(o,i){this.head=o,this.tail=i};function n(o){return new e(o,r)}function u(o){return function(i){return new e(o,i)}}function a(o){for(var i=[],f=o;f!==r;)i.push(f.head),f=f.tail;return i}return function(o){return function(i){return function(f){var m=function(D,A){return o(i(u)(f(D)))(A)},v=function(D,A,b){if(A===0)return D;var _=b[A-1];return new t(function(){var k=v(m(_,D),A-1,b);return k})};return function(D){for(var A=i(n)(f(D[D.length-1])),b=v(A,D.length-1,D);b instanceof t;)b=b.fn();return i(a)(b)}}}}}();var WS=function(t){return t};var qS=Ne;var HS=qt;var _R=zc();var KS=WS;var el=function(t){return t};var t_=function(t){return KS(Nh(t))};var nl=function(t){if(Vn(t)>0)return new $(KS(t));if(Jr)return w.value;throw new Error("Failed pattern match at Data.Array.NonEmpty (line 161, column 1 - line 161, column 58): "+[t.constructor.name])};var YS=function(t){return function(r){return t(el(r))}};var ZS=YS(Vn);var tx=function(){return YS(_R)};var lc={proof:function(t){return t},Coercible0:function(){}},rx=function(t){return t.proof};var pc=function(t){return t.reflectType};var nx=function(t){return pc(t)};var e_=ir;var ps=function(){return function(t){return t}};var Vb=function(t){return[t]};var Gb=function(t){var r=nx(t);return function(){return function(){return function(){return function(e){return US(r(e))}}}}};var Jb=[];var ux=function(){return function(){return function(t){return function(r){return tf(t)(r)}}}};function ax(t){return function(){var r={};for(var e in t)hasOwnProperty.call(t,e)&&(r[e]=t[e]);return r}}var wi={};function jb(t){return t()}function ix(t,r){var e={};for(var n in t)hasOwnProperty.call(t,n)&&(e[n]=r(t[n]));return e}function ox(t,r){var e={};for(var n in t)hasOwnProperty.call(t,n)&&(e[n]=r(n)(t[n]));return e}function fx(t){return function(r){return function(e){return function(n){var u=e;function a(i){return function(f){return r(f)(i)(n[i])}}for(var o in n)hasOwnProperty.call(n,o)&&(u=t(u)(a(o)));return u}}}}function n_(t){return function(r){var e=[];for(var n in r)hasOwnProperty.call(r,n)&&e.push(t(n)(r[n]));return e}}var bR=Object.keys||n_(function(t){return function(){return t}});function Xb(t){return function(r){return function(e){return function(){return e[t]=r,e}}}}var Qb=function(t){return function(r){return function(){return delete r[t],r}}};var cx=Xr(qt),hR=Q(Z);var Kb=n_(function(t){return function(r){return r}});var SR=ax;var lx=function(t){return function(r){return jb(function(){var n=SR(r)();return t(n)(),n})}};var px=function(t){return function(r){return ox(r,t)}};var So=function(t){return function(r){return lx(Xb(t)(r))}},vs={map:function(t){return function(r){return ix(r,t)}}},xR={mapWithIndex:px,Functor0:function(){return vs}},Yb=function(){return J};var ss=fx(Jf),_x=function(t){var r=K(t.Semigroup0()),e=Nt(t);return function(n){return ss(function(u){return function(a){return function(o){return r(u)(n(a)(o))}}})(e)}},u_={foldl:function(t){return ss(function(r){return function(e){return t(r)}})},foldr:function(t){return function(r){return function(e){return cx(t)(r)(Kb(e))}}},foldMap:function(t){var r=_x(t);return function(e){return r(M(e))}}},vx={foldlWithIndex:function(t){return ss(Gt(t))},foldrWithIndex:function(t){return function(r){return function(e){return cx(qc(t))(r)(n_(N.create)(e))}}},foldMapWithIndex:function(t){return _x(t)},Foldable0:function(){return u_}},CR={traverseWithIndex:function(t){var r=t.Apply0(),e=at(r),n=g(r.Functor0()),u=F(t);return function(a){return function(o){return ss(function(i){return function(f){return function(m){return e(n(Gt(So(f)))(i))(a(f)(m))}}})(u(wi))(o)}}},FunctorWithIndex0:function(){return xR},FoldableWithIndex1:function(){return vx},Traversable2:function(){return ul}},ul={traverse:function(t){var r=rf(CR)(t);return function(e){return r(M(e))}},sequence:function(t){return en(ul)(t)(hR)},Functor0:function(){return vs},Foldable1:function(){return u_}};var Zb=function(t){return lx(Qb(t))};function sx(t){var r={};for(var e in t)({}).hasOwnProperty.call(t,e)&&(r[e]=t[e]);return r}function mx(t){return function(r){return function(e){return e[t]=r,e}}}function Dx(t){return function(r){return function(e){return e[t]=r(e[t]),e}}}var a_=gi;var bx=function(){return function(){return function(t){var r=fe(t);return function(e){return function(n){return function(u){return Dx(r(e))(n)(u)}}}}}};var i_=function(){return function(){return function(t){var r=fe(t);return function(e){return function(n){return function(u){return mx(r(e))(n)(u)}}}}}};var tA=Z,ms=function(t){return function(r){return t(sx(r))}},Ax=Gt(ms)({});var TR=ka(me),FR=g(ft),yx=cf(qt);var ER=ct(Nc),MR=B(ir),eA=ps(),OR=h(Dr),wR=Tu(Da),IR=OD($g),xo=F(Ee),gx=g(Kn),PR=g(e_),hx=$u(Ee),uA=hx(Me),LR=ip(a_),RR=i_()()({reflectSymbol:function(){return"id"}}),NR=bx()()({reflectSymbol:function(){return"parent"}}),Sa=Ir(Kn),Sx=In(Ee),xx=hx(qt),UR=Q(Z),BR=g(ir),kx=K(zD($v)),WR=Dn(Ee)(qt),qR=K(Ne),HR=Qr(u_),zR=rr(Mv),dx=function(){function t(){}return t.value=new t,t}(),nA=function(){function t(){}return t.value=new t,t}(),VR=function(){function t(){}return t.value=new t,t}();var Zn=function(t){return function(r){return function(e){var n=function(u){return u(r)(e)};return function(u){if(u instanceof Yp)return yx(BR(Zn(t)(r)(e))(u.value0));if(u instanceof ri)return TR(FR(Zn(t)(r)(e))(u.value0));if(u instanceof _r)return n(t.toElt(u.value0));if(u instanceof Hb)return mr(function(a,o){var i=Fe(wi)(),f=a(u.value0,function(m){var v=t.ids(e)(),D=Fe(xo(void 0))(),A=t.ids(e)(),b=Fe(xo(void 0))(),_=Fe([])(),k=Fe(xo(void 0))(),V=gx(Eu.create)(function(){if(r.scope instanceof lf)return t.ids(e);if(r.scope instanceof Eu)return kx(xo(r.scope.value0))(kx(xo("!"))(t.ids(e)));throw new Error("Failed pattern match at Bolson.Control (line 547, column 17 - line 549, column 67): "+[r.scope.constructor.name])}())(),et=Fe(dx.value)(),Dt=a(m,function(Gr){var H=Ze(et)();if(Gr instanceof qb&&H instanceof nA){var Ht=Ze(_)();return WR(function(Wr){return function(){return o(t.doLogic(Gr.value0)(e)(Wr))}})(Ht)()}if(Gr instanceof Zc&&H instanceof nA){Sa(Cu(VR.value)(et))();var vt=function(){var fn=Ze(_)();xx(fn)(function(be){return uA(r.parent)(function(ue){return function(){return o(t.disconnectElement(e)({id:be,parent:ue,scope:V}))}})})();var On=Ze(D)();On();var ia=Ze(b)();return ia(),Sa(ja(Zb(v))(i))(),Sa(ja(Zb(A))(i))()};return Sa(Cu(vt)(k))(),vt()}if(Gr instanceof Yc&&H instanceof dx){Sa(Cu(nA.value)(et))();var qr=a(Zn(t)(function(){var Wr={};for(var fn in r)({}).hasOwnProperty.call(r,fn)&&(Wr[fn]=r[fn]);return Wr.scope=V,Wr.raiseId=function(On){return Sa(ja(qR([On]))(_))},Wr}())(e)(Gr.value0),o);return Sa(ja(So(A)(qr))(i))(),Sa(Cu(qr)(b))()}return void 0});Sa(Cu(Dt)(D))(),Sa(ja(So(v)(Dt))(i))();var vr=Ze(k)();return vr()});return function(){return ER(Ze(i))(HR(zR)(xo(void 0)))(),f()}});throw new Error("Failed pattern match at Bolson.Control (line 520, column 17 - line 610, column 20): "+[u.constructor.name])}}}},GR=function(){return function(){return function(){return function(t){return function(r){return function(e){return function(n){return function(u){return function(a){var o=function(i){return function(f){return mr(function(m,v){var D=MS(MR(eA(u))({id:"",entity:new ri(OR)}))(),A=yx(wR(function(vr){return IR(function(Gr){return function(H){return H instanceof _r?function(Ht){return Ht(function(){var vt={};for(var qr in i)({}).hasOwnProperty.call(i,qr)&&(vt[qr]=i[qr]);return vt.parent=w.value,vt.scope=r(i.scope),vt.raiseId=function(Wr){return OS(vr)({id:Wr,entity:H})(D)},vt}())(f)}(n.toElt(H.value0)):Gr(n.wrapElt(H))}})})(eA(u))),b=m(A,v),_=Fe(xo(void 0))(),k=gx(J)(wS(D))(),V=PR(function(vr){return function(Gr){return new _r(n.fromEltO1(function(H){return function(Ht){return mr(function(vt,qr){return H.raiseId(vr.id)(),uA(H.parent)(function(Wr){return function(){return qr(n.giveNewParent(Ht)(ms(LR(RR(d.value)(vr.id))(NR(d.value)(M(Wr))))(H))(vr.entity)(Gr))}})(),xo(void 0)})}}))}})(k),et=Zn(e)(i)(f)(a(V)),Dt=m(et,v);return Sa(Cu(Dt)(_))(),function(){b(),Sx(!t)(xx(eA(k))(function(H){return function(){return v(n.deleteFromCache(f)({id:H.id}))}}))();var Gr=Ze(_)();return Gr()}})}};return new _r(n.fromEltO2(o))}}}}}}}}},JR=GR()()();var aA=function(){return function(){return function(){return function(t){return function(r){return function(e){return function(n){return JR(!1)(UR)(t)(r)(e)(n)}}}}}}};var Cx=function(t){return function(r){return function(e){var n=function(u){return function(a){return mr(function(o,i){var f=Fe(w.value)(),m=e(new _r(r.fromElt(function(v){return function(D){return mr(function(A,b){return function(){var k=Ze(f)();if(k instanceof w)return void 0;if(k instanceof $)return uA(v.parent)(function(V){return Sx(k.value0!==V)(function(){return v.raiseId(k.value0)(),b(r.connectToParent(a)({id:k.value0,parent:V}))})})();throw new Error("Failed pattern match at Bolson.Control (line 635, column 27 - line 643, column 16): "+[k.constructor.name])}(),xo(void 0)})}})));return o(Zn(t)(function(){var v={};for(var D in u)({}).hasOwnProperty.call(u,D)&&(v[D]=u[D]);return v.parent=u.parent,v.scope=u.scope,v.raiseId=function(A){return function(){return u.raiseId(A)(),Sa(Cu(new $(A))(f))()}},v}())(a)(m),i)})}};return new _r(r.fromElt(n))}}};var jR=Q(Z);var Wn={dimap:function(t){return function(r){return function(e){return function(n){return r(e(t(n)))}}}}},Ii=function(t){return t.dimap},Ju=function(t){var r=Ii(t);return function(e){return r(e)(jR)}};var XR=Wb(),il=g(ft),QR=Ju(Wn),KR=ie(),YR=dt(go),ZR=tn(),tN=F(Ee),iA=F(j),rN=h(Dr),eN=cf(qt);var Pi=function(){return function(t){var r=XR(t);return function(e){return function(n){var u=r(e)(n),a=il(function(i){return i})(u),o=il(function(i){return i})(a);return new ri(o)}}}};var $x=function(t){return function(r){var e=function(n){var u=function(a){return a instanceof _r?new _r(QR(function(o){return{pos:t,dynFamily:o.dynFamily,ez:o.ez,parent:o.parent,raiseId:o.raiseId,scope:o.scope}})(a.value0)):a instanceof ri?new ri(il(u)(a.value0)):a};return u(n)};return e(r)}},Tx=function(t){return $x($.create(t))};var oA=function(){return Zc.value}(),nN=function(){return{doLogic:function(t){return function(r){return function(e){return r.sendToPos({id:e,pos:t})}}},ids:function(t){return function(r){return r.ids}(KR(t))},disconnectElement:function(t){return function(r){return t.disconnectElement({id:r.id,scope:r.scope,parent:r.parent,scopeEq:YR})}},toElt:function(t){return t}}},uN=nN(),Fx=function(t){var r=function(n){return new Yc(n)},e=$x(w.value)(t);return r(function(n){return n}(e))};var Ex=function(t){return Nb(t)};var Be=function(t){var r=function(n){return new ri(ZR(n))},e=Ex(t);return r(il(function(n){return n})(e))},Mx=function(t){return Be(Zf(t))},aN=function(t){return function(r){return function(e){return Zn(uN)(t)(r)(function(n){return n}(e))}}},iN=function(t){return function(r){var e=function(u){return function(a){return function(o){return mr(function(i,f){var m=o.ids();a.raiseId(m)();var v=function(){if(a.parent instanceof w){var A=o.ids();return new N(iA(o.makeElement({id:A,parent:w.value,scope:a.scope,tag:"div",pos:w.value,dynFamily:w.value})),A)}if(a.parent instanceof $)return new N(rN,a.parent.value0);throw new Error("Failed pattern match at Deku.Core (line 451, column 38 - line 467, column 40): "+[a.parent.constructor.name])}(),D=i(eN([v.value0,iA(o.makeDynBeacon({id:m,parent:new $(v.value1),scope:a.scope,dynFamily:a.dynFamily,pos:a.pos})),iA(o.attributeParent({id:m,parent:v.value1,pos:a.pos,dynFamily:a.dynFamily,ez:a.ez})),aN({parent:new $(v.value1),scope:a.scope,ez:!1,raiseId:function(A){return tN(void 0)},pos:w.value,dynFamily:new $(m)})(o)(u)]),f);return function(){return f(o.removeDynBeacon({id:m})),D()}})}}},n=function(u){return new _r(e(u))};return n(function(u){return u}(t(r)))}},Ox=function(){var t=function(e){return Zp(e)},r=function(e){return t(il(il(function(n){return n}))(e))};return iN(r)}();var Ds=g(ft);var bs=cf(qt),_c=F(j),Ix=h(Dr),Px=F(Ee),fN=Qc(me),cN=ka(me),lN=qp(ya),pN=dt(eo),Lx=tn(),_N=ie(),vN=dt(go),Rx=gt(Wt),sN=K(Ne),mN=Tu(Da),wx=g(Qe);var DN=function(t){return function(r){return function(e){return Ds(function(n){return t.setText(function(u){return{id:r,text:u}}(n))})(e)}}},bN=function(t){return function(r){return function(e){return Ds(function(n){return function(u){if(u.value instanceof jp)return t.setProp({id:r,key:u.key,value:u.value.value0});if(u.value instanceof Xp)return t.setCb({id:r,key:u.key,value:u.value.value0});if(u.value instanceof us)return t.unsetAttribute({id:r,key:u.key});throw new Error("Failed pattern match at Deku.Control (line 72, column 28 - line 75, column 47): "+[u.value.constructor.name])}(as(n))})(e)}}};var Ve=function(t){var r=function(n){return function(u){return mr(function(a,o){var i=u.ids();n.raiseId(i)();var f=a(bs([_c(u.makeText({id:i,parent:n.parent,pos:n.pos,scope:n.scope,dynFamily:n.dynFamily})),DN(u)(i)(t),yt(Ix)(function(m){return _c(u.attributeParent({id:i,parent:m,pos:n.pos,dynFamily:n.dynFamily,ez:n.ez}))})(n.parent)]),o);return function(){return o(u.deleteFromCache({id:i})),f()}})}},e=new _r(r);return e},S=function(t){return Ve(_c(t))},Nx=function(t){return function(r){var e=function(){var n=function(u){return function(a){return new N(u+1|0,new N(a,u))}};return fN(n)(0)}();return Ox(cN(ti(e(r))(function(n){return Ds(function(u){return bs([Ds(M(oA))(lN(function(){var a=pN(u.value1+1|0);return function(o){return a(Ue(o))}}())(n)),_c(Fx(Lx(t(u.value0))))])})(n)})))}};var AN=function(){return{doLogic:function(t){return function(r){return function(e){return r.sendToPos({id:e,pos:t})}}},ids:function(t){return function(r){return r.ids}(_N(t))},disconnectElement:function(t){return function(r){return t.disconnectElement({id:r.id,scope:r.scope,parent:r.parent,scopeEq:vN})}},toElt:function(t){return t}}},kN=AN();var Ux=function(t){return function(r){return function(e){return Zn(kN)(t)(r)(e)}}},Bx=function(t){return function(r){var e=function(n){return function(u){return mr(function(a,o){return a(Rx(_c(u.makeRoot({id:"deku-root",root:t})))(Ux({parent:new $("deku-root"),scope:new Eu("rootScope"),raiseId:function(i){return Px(void 0)},ez:!0,pos:w.value,dynFamily:w.value})(u)(n)),o)})}};return e(r)}},dN=function(t){return function(r){return function(e){var n=function(u){return function(a){return mr(function(o,i){var f=a.ids();u.raiseId(f)();var m=o(Rx(bs(sN([_c(a.makeElement({id:f,parent:u.parent,scope:u.scope,tag:t,pos:u.pos,dynFamily:u.dynFamily})),bN(a)(f)(r)])(yt([])(function(v){return[_c(a.attributeParent({id:f,parent:v,pos:u.pos,dynFamily:u.dynFamily,ez:u.ez}))]})(u.parent))))(Ux({parent:new $(f),scope:u.scope,ez:!0,raiseId:function(v){return Px(void 0)},pos:w.value,dynFamily:w.value})(a)(e)),i);return function(){return i(a.deleteFromCache({id:f})),m()}})}};return n}}},E=function(t){return function(r){return function(e){var n=function(a){return a.length===0?Ix:a.length===1?a[0]:bs(a)},u=function(a){return new _r(dN(t)(n(r))(ze(Lx(a))))};return u(mN(wx(wx(function(a){return a}))(Tx))(e))}}};var ee=function(){function t(){}return t.value=new t,t}();var ke={attr:function(t){return function(r){return c({key:"click",value:G(r)})}}};var Mt=function(){function t(){}return t.value=new t,t}();var As={attr:function(t){return function(r){return c({key:"style",value:s(r)})}}};var Wx={attr:function(t){return function(r){return c({key:"style",value:s(r)})}}};var kr={attr:function(t){return function(r){return c({key:"style",value:s(r)})}}};var qx={attr:function(t){return function(r){return c({key:"style",value:s(r)})}}},ol={attr:function(t){return function(r){return c({key:"style",value:s(r)})}}};var Hx={attr:function(t){return function(r){return c({key:"style",value:s(r)})}}};var zx={attr:function(t){return function(r){return c({key:"style",value:s(r)})}}};var fA=E("a");var sr=E("div"),Rr=sr(h(O));var cl=E("span"),cA=cl(h(O));var Ri=function(t){return function(r){return t(r)}};var tu=function(){var t={},r="Pure",e="Throw",n="Catch",u="Sync",a="Async",o="Bind",i="Bracket",f="Fork",m="Sequential",v="Map",D="Apply",A="Alt",b="Cons",_="Resume",k="Release",V="Finalizer",et="Finalized",Dt="Forked",vr="Fiber",Gr="Thunk";function H(Ct,zr,Pe,Yr){this.tag=Ct,this._1=zr,this._2=Pe,this._3=Yr}function Ht(Ct){var zr=function(Pe,Yr,zt){return new H(Ct,Pe,Yr,zt)};return zr.tag=Ct,zr}function vt(Ct){return new H(r,void 0)}function qr(Ct){try{Ct()}catch(zr){setTimeout(function(){throw zr},0)}}function Wr(Ct,zr,Pe){try{return zr(Pe())}catch(Yr){return Ct(Yr)}}function fn(Ct,zr,Pe){try{return zr(Pe)()}catch(Yr){return Pe(Ct(Yr))(),vt}}var On=function(){var Ct=1024,zr=0,Pe=0,Yr=new Array(Ct),zt=!1;function ot(){var jr;for(zt=!0;zr!==0;)zr--,jr=Yr[Pe],Yr[Pe]=void 0,Pe=(Pe+1)%Ct,jr();zt=!1}return{isDraining:function(){return zt},enqueue:function(jr){var hr,Ce;zr===Ct&&(Ce=zt,ot(),zt=Ce),Yr[(Pe+zr)%Ct]=jr,zr++,zt||ot()}}}();function ia(Ct){var zr={},Pe=0,Yr=0;return{register:function(zt){var ot=Pe++;zt.onComplete({rethrow:!0,handler:function(jr){return function(){Yr--,delete zr[ot]}}})(),zr[ot]=zt,Yr++},isEmpty:function(){return Yr===0},killAll:function(zt,ot){return function(){if(Yr===0)return ot();var jr=0,hr={};function Ce(ae){hr[ae]=zr[ae].kill(zt,function(qe){return function(){delete hr[ae],jr--,Ct.isLeft(qe)&&Ct.fromLeft(qe)&&setTimeout(function(){throw Ct.fromLeft(qe)},0),jr===0&&ot()}})()}for(var yn in zr)zr.hasOwnProperty(yn)&&(jr++,Ce(yn));return zr={},Pe=0,Yr=0,function(ae){return new H(u,function(){for(var qe in hr)hr.hasOwnProperty(qe)&&hr[qe]()})}}}}}var be=0,ue=1,Wu=2,to=3,Ec=4,lu=5,dn=6;function Vf(Ct,zr,Pe){var Yr=0,zt=be,ot=Pe,jr=null,hr=null,Ce=null,yn=null,ae=null,qe=0,Gf=0,oa=null,Vo=!0;function Go(Vt){for(var jt,Nr,Hr;;)switch(jt=null,Nr=null,Hr=null,zt){case Wu:zt=ue;try{ot=Ce(ot),yn===null?Ce=null:(Ce=yn._1,yn=yn._2)}catch(wn){zt=lu,jr=Ct.left(wn),ot=null}break;case to:Ct.isLeft(ot)?(zt=lu,jr=ot,ot=null):Ce===null?zt=lu:(zt=Wu,ot=Ct.fromRight(ot));break;case ue:switch(ot.tag){case o:Ce&&(yn=new H(b,Ce,yn)),Ce=ot._2,zt=ue,ot=ot._1;break;case r:Ce===null?(zt=lu,ot=Ct.right(ot._1)):(zt=Wu,ot=ot._1);break;case u:zt=to,ot=Wr(Ct.left,Ct.right,ot._1);break;case a:zt=Ec,ot=fn(Ct.left,ot._1,function(wn){return function(){Yr===Vt&&(Yr++,On.enqueue(function(){Yr===Vt+1&&(zt=to,ot=wn,Go(Yr))}))}});return;case e:zt=lu,jr=Ct.left(ot._1),ot=null;break;case n:Ce===null?ae=new H(b,ot,ae,hr):ae=new H(b,ot,new H(b,new H(_,Ce,yn),ae,hr),hr),Ce=null,yn=null,zt=ue,ot=ot._1;break;case i:qe++,Ce===null?ae=new H(b,ot,ae,hr):ae=new H(b,ot,new H(b,new H(_,Ce,yn),ae,hr),hr),Ce=null,yn=null,zt=ue,ot=ot._1;break;case f:zt=to,jt=Vf(Ct,zr,ot._2),zr&&zr.register(jt),ot._1&&jt.run(),ot=Ct.right(jt);break;case m:zt=ue,ot=ro(Ct,zr,ot._1);break}break;case lu:if(Ce=null,yn=null,ae===null)zt=dn,ot=hr||jr||ot;else switch(jt=ae._3,Hr=ae._1,ae=ae._2,Hr.tag){case n:hr&&hr!==jt&&qe===0?zt=lu:jr&&(zt=ue,ot=Hr._2(Ct.fromLeft(jr)),jr=null);break;case _:hr&&hr!==jt&&qe===0||jr?zt=lu:(Ce=Hr._1,yn=Hr._2,zt=Wu,ot=Ct.fromRight(ot));break;case i:qe--,jr===null&&(Nr=Ct.fromRight(ot),ae=new H(b,new H(k,Hr._2,Nr),ae,jt),(hr===jt||qe>0)&&(zt=ue,ot=Hr._3(Nr)));break;case k:ae=new H(b,new H(et,ot,jr),ae,hr),zt=ue,hr&&hr!==jt&&qe===0?ot=Hr._1.killed(Ct.fromLeft(hr))(Hr._2):jr?ot=Hr._1.failed(Ct.fromLeft(jr))(Hr._2):ot=Hr._1.completed(Ct.fromRight(ot))(Hr._2),jr=null,qe++;break;case V:qe++,ae=new H(b,new H(et,ot,jr),ae,hr),zt=ue,ot=Hr._1;break;case et:qe--,zt=lu,ot=Hr._1,jr=Hr._2;break}break;case dn:for(var Re in oa)oa.hasOwnProperty(Re)&&(Vo=Vo&&oa[Re].rethrow,qr(oa[Re].handler(ot)));oa=null,hr&&jr?setTimeout(function(){throw Ct.fromLeft(jr)},0):Ct.isLeft(ot)&&Vo&&setTimeout(function(){if(Vo)throw Ct.fromLeft(ot)},0);return;case be:zt=ue;break;case Ec:return}}function Le(Vt){return function(){if(zt===dn)return Vo=Vo&&Vt.rethrow,Vt.handler(ot)(),function(){};var jt=Gf++;return oa=oa||{},oa[jt]=Vt,function(){oa!==null&&delete oa[jt]}}}function ur(Vt,jt){return function(){if(zt===dn)return jt(Ct.right(void 0))(),function(){};var Nr=Le({rethrow:!1,handler:function(){return jt(Ct.right(void 0))}})();switch(zt){case be:hr=Ct.left(Vt),zt=dn,ot=hr,Go(Yr);break;case Ec:hr===null&&(hr=Ct.left(Vt)),qe===0&&(zt===Ec&&(ae=new H(b,new H(V,ot(Vt)),ae,hr)),zt=lu,ot=null,jr=null,Go(++Yr));break;default:hr===null&&(hr=Ct.left(Vt)),qe===0&&(zt=lu,ot=null,jr=null)}return Nr}}function Fr(Vt){return function(){var jt=Le({rethrow:!1,handler:Vt})();return zt===be&&Go(Yr),jt}}return{kill:ur,join:Fr,onComplete:Le,isSuspended:function(){return zt===be},run:function(){zt===be&&(On.isDraining()?Go(Yr):On.enqueue(function(){Go(Yr)}))}}}function Sv(Ct,zr,Pe,Yr){var zt=0,ot={},jr=0,hr={},Ce=new Error("[ParAff] Early exit"),yn=null,ae=t;function qe(Le,ur,Fr){var Vt=ur,jt=null,Nr=null,Hr=0,Re={},wn,ap;t:for(;;)switch(wn=null,Vt.tag){case Dt:if(Vt._3===t&&(wn=ot[Vt._1],Re[Hr++]=wn.kill(Le,function(Mw){return function(){Hr--,Hr===0&&Fr(Mw)()}})),jt===null)break t;Vt=jt._2,Nr===null?jt=null:(jt=Nr._1,Nr=Nr._2);break;case v:Vt=Vt._2;break;case D:case A:jt&&(Nr=new H(b,jt,Nr)),jt=Vt,Vt=Vt._1;break}if(Hr===0)Fr(Ct.right(void 0))();else for(ap=0,wn=Hr;ap<wn;ap++)Re[ap]=Re[ap]();return Re}function Gf(Le,ur,Fr){var Vt,jt,Nr,Hr,Re,wn;Ct.isLeft(Le)?(Vt=Le,jt=null):(jt=Le,Vt=null);t:for(;;){if(Nr=null,Hr=null,Re=null,wn=null,yn!==null)return;if(ur===null){Yr(Vt||jt)();return}if(ur._3!==t)return;switch(ur.tag){case v:Vt===null?(ur._3=Ct.right(ur._1(Ct.fromRight(jt))),jt=ur._3):ur._3=Vt;break;case D:if(Nr=ur._1._3,Hr=ur._2._3,Vt){if(ur._3=Vt,Re=!0,wn=jr++,hr[wn]=qe(Ce,Vt===Nr?ur._2:ur._1,function(){return function(){delete hr[wn],Re?Re=!1:Fr===null?Gf(Vt,null,null):Gf(Vt,Fr._1,Fr._2)}}),Re){Re=!1;return}}else{if(Nr===t||Hr===t)return;jt=Ct.right(Ct.fromRight(Nr)(Ct.fromRight(Hr))),ur._3=jt}break;case A:if(Nr=ur._1._3,Hr=ur._2._3,Nr===t&&Ct.isLeft(Hr)||Hr===t&&Ct.isLeft(Nr))return;if(Nr!==t&&Ct.isLeft(Nr)&&Hr!==t&&Ct.isLeft(Hr))Vt=jt===Nr?Hr:Nr,jt=null,ur._3=Vt;else if(ur._3=jt,Re=!0,wn=jr++,hr[wn]=qe(Ce,jt===Nr?ur._2:ur._1,function(){return function(){delete hr[wn],Re?Re=!1:Fr===null?Gf(jt,null,null):Gf(jt,Fr._1,Fr._2)}}),Re){Re=!1;return}break}Fr===null?ur=null:(ur=Fr._1,Fr=Fr._2)}}function oa(Le){return function(ur){return function(){delete ot[Le._1],Le._3=ur,Gf(ur,Le._2._1,Le._2._2)}}}function Vo(){var Le=ue,ur=Pe,Fr=null,Vt=null,jt,Nr;t:for(;;)switch(jt=null,Nr=null,Le){case ue:switch(ur.tag){case v:Fr&&(Vt=new H(b,Fr,Vt)),Fr=new H(v,ur._1,t,t),ur=ur._2;break;case D:Fr&&(Vt=new H(b,Fr,Vt)),Fr=new H(D,t,ur._2,t),ur=ur._1;break;case A:Fr&&(Vt=new H(b,Fr,Vt)),Fr=new H(A,t,ur._2,t),ur=ur._1;break;default:Nr=zt++,Le=lu,jt=ur,ur=new H(Dt,Nr,new H(b,Fr,Vt),t),jt=Vf(Ct,zr,jt),jt.onComplete({rethrow:!1,handler:oa(ur)})(),ot[Nr]=jt,zr&&zr.register(jt)}break;case lu:if(Fr===null)break t;Fr._1===t?(Fr._1=ur,Le=ue,ur=Fr._2,Fr._2=t):(Fr._2=ur,ur=Fr,Vt===null?Fr=null:(Fr=Vt._1,Vt=Vt._2))}for(ae=ur,Nr=0;Nr<zt;Nr++)ot[Nr].run()}function Go(Le,ur){yn=Ct.left(Le);var Fr;for(var Vt in hr)if(hr.hasOwnProperty(Vt)){Fr=hr[Vt];for(Vt in Fr)Fr.hasOwnProperty(Vt)&&Fr[Vt]()}hr=null;var jt=qe(Le,ae,ur);return function(Nr){return new H(a,function(Hr){return function(){for(var Re in jt)jt.hasOwnProperty(Re)&&jt[Re]();return vt}})}}return Vo(),function(Le){return new H(a,function(ur){return function(){return Go(Le,ur)}})}}function ro(Ct,zr,Pe){return new H(a,function(Yr){return function(){return Sv(Ct,zr,Pe,Yr)}})}return H.EMPTY=t,H.Pure=Ht(r),H.Throw=Ht(e),H.Catch=Ht(n),H.Sync=Ht(u),H.Async=Ht(a),H.Bind=Ht(o),H.Bracket=Ht(i),H.Fork=Ht(f),H.Seq=Ht(m),H.ParMap=Ht(v),H.ParApply=Ht(D),H.ParAlt=Ht(A),H.Fiber=Vf,H.Supervisor=ia,H.Scheduler=On,H.nonCanceler=vt,H}(),Gx=tu.Pure,gN=tu.Throw;function Jx(t){return function(r){return r.tag===tu.Pure.tag?tu.Pure(t(r._1)):tu.Bind(r,function(e){return tu.Pure(t(e))})}}function jx(t){return function(r){return tu.Bind(t,r)}}var Xx=tu.Sync;function Qx(t){return function(r){return tu.ParMap(t,r)}}function Kx(t){return function(r){return tu.ParApply(t,r)}}function Yx(t){return function(r){return tu.ParAlt(t,r)}}var ll=tu.Async;function Zx(t,r){return function(){return tu.Fiber(t,null,r)}}var hN=function(){function t(e,n){return e===0&&typeof setImmediate<"u"?setImmediate(n):setTimeout(n,e)}function r(e,n){return e===0&&typeof clearImmediate<"u"?clearImmediate(n):clearTimeout(n)}return function(e,n){return tu.Async(function(u){return function(){var a=t(n,u(e()));return function(){return tu.Sync(function(){return e(r(n,a))})}}})}}(),tC=tu.Seq;function ni(t){return new Error(t)}function o_(t){return function(){throw t}}function ks(t){return function(r){return function(){try{return r()}catch(e){return e instanceof Error||Object.prototype.toString.call(e)==="[object Error]"?t(e)():t(new Error(e.toString()))()}}}}var ui=function(t){return t.throwError};var CN={throwError:o_,Monad0:function(){return ao}};var eC={catchError:Gt(ks),MonadThrow0:function(){return CN}};var Co=function(t){return t.catchError};var ds=function(t){var r=Co(t),e=t.MonadThrow0().Monad0(),n=g(e.Bind1().Apply0().Functor0()),u=F(e.Applicative0());return function(a){return r(n(xt.create)(a))(function(o){return u(St.create(o))})}};var $o=function(t){return t.state};var ne={liftEffect:Q(Z),Monad0:function(){return ao}},de=function(t){return t.liftEffect};var TN=g(Qf);var xs=function(t){return t};var Cs=function(t){return t};var $s=function(t){return function(r){return t(r)}},v_=function(t){var r=g(t);return{map:function(e){return $s(r(TN(e)))}}};var pA=function(t){return{Applicative0:function(){return s_(t)},Bind1:function(){return _A(t)}}},_A=function(t){var r=ct(t.Bind1()),e=F(t.Applicative0());return{bind:function(n){return function(u){return r(n)(jn(function(a){return e(St.create(a))})(function(a){var o=u(a);return o}))}},Apply0:function(){return nC(t)}}},nC=function(t){var r=v_(t.Bind1().Apply0().Functor0());return{apply:Gn(pA(t)),Functor0:function(){return r}}},s_=function(t){return{pure:function(){var r=F(t.Applicative0());return function(e){return xs(r(xt.create(e)))}}(),Apply0:function(){return nC(t)}}};var uC=function(t){var r=pA(t);return{throwError:function(){var e=F(t.Applicative0());return function(n){return xs(e(St.create(n)))}}(),Monad0:function(){return r}}};var vA=function(t){var r=K(t);return function(e){var n=e.Bind1(),u=ct(n),a=F(e.Applicative0()),o=v_(n.Apply0().Functor0());return{alt:function(i){return function(f){return u(i)(function(m){if(m instanceof xt)return a(new xt(m.value0));if(m instanceof St)return u(f)(function(v){if(v instanceof xt)return a(new xt(v.value0));if(v instanceof St)return a(new St(r(m.value0)(v.value0)));throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): "+[v.constructor.name])});throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): "+[m.constructor.name])})}},Functor0:function(){return o}}}};var ai=function(t){return t.sequential},Qu=function(t){return t.parallel};var PN=Q(Z),LN=function(t){var r=ai(t),e=Dn(t.Applicative1()),n=Qu(t);return function(u){var a=e(u);return function(o){var i=a(function(f){return n(o(f))});return function(f){return r(i(f))}}}},iC=function(t){var r=ai(t),e=t.Applicative1(),n=Qu(t);return function(u){var a=en(u)(e);return function(o){var i=a(function(f){return n(o(f))});return function(f){return r(i(f))}}}},oC=function(t){var r=LN(t);return function(e){return r(e)(PN)}};var cC=dh()();var RN=function(t){return t};var pC=function(t){return t};var vl=function(t){return t.toDuration};var _C={fromDuration:cC(RN)(function(t){return t*1e3}),toDuration:cC(pC)(function(t){return t/1e3})};var vC=function(t,r,e){var n=0,u;return function(a){if(n===2)return u;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+a+")",r,a);return n=1,u=e(),n=2,u}},UN=F(Bt),sC=Ir(L),mC=g(L);var BN=function(t){return t};var ml={map:Qx},To={map:Jx};var WN=function(){var t=function(n){if(n instanceof xt)return n.value0;if(n instanceof St)return Fu("unsafeFromRight: Left");throw new Error("Failed pattern match at Effect.Aff (line 412, column 21 - line 414, column 54): "+[n.constructor.name])},r=function(n){if(n instanceof St)return n.value0;if(n instanceof xt)return Fu("unsafeFromLeft: Right");throw new Error("Failed pattern match at Effect.Aff (line 407, column 20 - line 409, column 55): "+[n.constructor.name])},e=function(n){if(n instanceof St)return!0;if(n instanceof xt)return!1;throw new Error("Failed pattern match at Effect.Aff (line 402, column 12 - line 404, column 21): "+[n.constructor.name])};return{isLeft:e,fromLeft:r,fromRight:t,left:St.create,right:xt.create}}(),qN=function(t){return Zx(WN,t)},Ca=function(t){return function(){var e=qN(t)();return e.run(),e}},ii=function(t){return sC(Ca(t))};var D_={apply:Kx,Functor0:function(){return ml}};var DA={Applicative0:function(){return ru},Bind1:function(){return qn}},qn={bind:jx,Apply0:function(){return bA(0)}},ru={pure:Gx,Apply0:function(){return bA(0)}},bA=vC("applyAff","Effect.Aff",function(){return{apply:Gn(DA),Functor0:function(){return To}}}),DC=bA(73),bC=F(ru),HN=ct(qn);var Ge={liftEffect:Xx,Monad0:function(){return DA}},mA=de(Ge),AC=function(t){return BN(M(mA(t)))},kC=function(t){return ll(function(r){return mC(AC)(t.join(r))})};var AA=function(t){return function(r){return HN(mA(r.isSuspended))(function(e){return e?mA(sC(r.kill(t,M(UN(void 0))))):ll(function(n){return mC(AC)(r.kill(t,n))})})}};var Ku={parallel:J,sequential:tC,Monad0:function(){return DA},Applicative1:function(){return zN(0)}},zN=vC("applicativeParAff","Effect.Aff",function(){return{pure:function(){var t=Qu(Ku);return function(r){return t(bC(r))}}(),Apply0:function(){return D_}}});var VN=oC(Ku)(qt);var GN={append:function(t){return function(r){return function(e){return VN([t(e),r(e)])}}}};var JN=M(bC(void 0)),dC={mempty:JN,Semigroup0:function(){return GN}};var yC={alt:Yx,Functor0:function(){return ml}};var gC=g(ft),jN=gt(Wt),XN=F(j);var Dl=Mx;var QN=function(t){return function(r){var e=ti(t)(r),n=gC(function(a){return a})(e),u=tl(gC(function(a){return a})(n));return u}},hC=function(t){return function(r){return Ri(Dl)(function(e){return Ri(QN(jN(e.value1)(XN(t))))(function(n){return r(new N(e.value0,n))})})}};var Es=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}();var kA=function(t){var r=!1,e;function n(u){if(u.value0 instanceof wr&&u.value1 instanceof wr)return r=!0,w.value;if(u.value0 instanceof wr){t=new Es(kb(u.value1),wr.value);return}if(u.value0 instanceof Et)return r=!0,new $(new N(u.value0.value0,new Es(u.value0.value1,u.value1)));throw new Error("Failed pattern match at Data.CatQueue (line 82, column 1 - line 82, column 63): "+[u.constructor.name])}for(;!r;)e=n(t);return e},dA=function(t){return function(r){return new Es(t.value0,new Et(r,t.value1))}};var xC=function(t){return t.value0 instanceof wr&&t.value1 instanceof wr};var yA=function(){return new Es(wr.value,wr.value)}();var bl=function(){function t(){}return t.value=new t,t}(),Ms=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}();var $C=function(t){return function(r){if(t instanceof bl)return r;if(r instanceof bl)return t;if(t instanceof Ms)return new Ms(t.value0,dA(t.value1)(r));throw new Error("Failed pattern match at Data.CatList (line 108, column 1 - line 108, column 54): "+[t.constructor.name,r.constructor.name])}},ZN=function(t){return function(r){return function(e){var n=function(a){return function(o){return function(i){var f=a,m=o,v=!1,D;function A(b,_,k){if(k instanceof wr)return v=!0,_;if(k instanceof Et){f=b,m=b(_)(k.value0),i=k.value1;return}throw new Error("Failed pattern match at Data.CatList (line 124, column 3 - line 124, column 59): "+[b.constructor.name,_.constructor.name,k.constructor.name])}for(;!v;)D=A(f,m,i);return D}}},u=function(a){return function(o){var i=a,f=!1,m;function v(D,A){var b=kA(D);if(b instanceof w)return f=!0,n(function(_){return function(k){return k(_)}})(r)(A);if(b instanceof $){i=b.value0.value1,o=new Et(t(b.value0.value0),A);return}throw new Error("Failed pattern match at Data.CatList (line 120, column 14 - line 122, column 67): "+[b.constructor.name])}for(;!f;)m=v(i,o);return m}};return u(e)(wr.value)}}},TC=function(t){if(t instanceof bl)return w.value;if(t instanceof Ms)return new $(new N(t.value0,function(){var r=xC(t.value1);return r?bl.value:ZN($C)(bl.value)(t.value1)}()));throw new Error("Failed pattern match at Data.CatList (line 99, column 1 - line 99, column 61): "+[t.constructor.name])};var FC=function(){return bl.value}(),EC=$C;var MC={append:EC};var OC=function(t){return function(r){return EC(t)(new Ms(r,yA))}};var rU=function(t,r,e){var n=0,u;return function(a){if(n===2)return u;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+a+")",r,a);return n=1,u=e(),n=2,u}},eU=K(MC);var gA=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),Os=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),b_=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),nU=function(t){var r=!1,e;function n(u){var a=function(f){return f},o=function(f){return function(m){return new gA(f.value0,eU(f.value1)(m))}};if(u.value0 instanceof Os){var i=TC(u.value1);if(i instanceof w)return r=!0,new Os(u.value0.value0);if(i instanceof $){t=o(a(i.value0.value0)(u.value0.value0))(i.value0.value1);return}throw new Error("Failed pattern match at Control.Monad.Free (line 227, column 7 - line 231, column 64): "+[i.constructor.name])}if(u.value0 instanceof b_)return r=!0,new b_(u.value0.value0,function(f){return o(u.value0.value1(f))(u.value1)});throw new Error("Failed pattern match at Control.Monad.Free (line 225, column 3 - line 233, column 56): "+[u.value0.constructor.name])}for(;!r;)e=n(t);return e};var uU=function(t){return function(r){return function(e){var n=nU(e);if(n instanceof Os)return r(n.value0);if(n instanceof b_)return t(n.value0)(n.value1);throw new Error("Failed pattern match at Control.Monad.Free (line 213, column 17 - line 215, column 20): "+[n.constructor.name])}}},wC=function(t){var r=g(t);return uU(function(e){return function(n){return new St(r(n)(e))}})(xt.create)},hA=function(t){return new gA(t,FC)},IC=function(t){return hA(new b_(t,J))};var aU={Applicative0:function(){return SA},Bind1:function(){return ws}},iU={map:function(t){return function(r){return cn(ws)(function(){var e=F(SA);return function(n){return e(t(n))}}())(r)}}},ws={bind:function(t){return function(r){return new gA(t.value0,OC(t.value1)(r))}},Apply0:function(){return PC(0)}},SA={pure:function(t){return hA(Os.create(t))},Apply0:function(){return PC(0)}},PC=rU("freeApply","Control.Monad.Free",function(){return{apply:Gn(aU),Functor0:function(){return iU}}});var oU=F(SA);var Hn=function(t){return hA(new b_(t,function(r){return oU(r)}))};var RC=t=>r=>e=>()=>{if(e.units[r.id]){let n=e.units[r.parent].main;e.units[r.id].main&&e.units[r.id].main.parentNode||e.units[r.id].startBeacon&&e.units[r.id].startBeacon.parentNode||(r.ez?(()=>(e.units[r.id].main?n.appendChild(e.units[r.id].main):(n.appendChild(e.units[r.id].startBeacon),n.appendChild(e.units[r.id].endBeacon)),!0))():t(r.pos)(a=>()=>t(r.dynFamily)(o=>()=>{for(var i=0,f=0,m;f<n.childNodes.length;){if(n.childNodes[f].nodeType===8&&n.childNodes[f].nodeValue==="%-%"+o){f+=1;break}f++}let v=A=>{let b=n.childNodes[A];e.units[r.id].startBeacon?(n.insertBefore(e.units[r.id].startBeacon,b),n.insertBefore(e.units[r.id].endBeacon,b)):n.insertBefore(e.units[r.id].main,b)};for(;f<n.childNodes.length;){var D;if((D=n.childNodes[f].$dekuId)&&t(e.units[D].dynFamily)(b=>()=>t(e.units[D].pos)(k=>()=>o===b&&a<=k?(v(f),!0):!1)())())return!0;if(i===a||n.childNodes[f].nodeType===8&&n.childNodes[f].nodeValue==="%-%"+o+"%-%")return v(f),!0;n.childNodes[f].nodeType===8&&n.childNodes[f].nodeValue.substring(0,3)==="%-%"&&!m&&(m=n.childNodes[f].nodeValue+"%-%"),m||i++,n.childNodes[f].nodeType===8&&n.childNodes[f].nodeValue===m&&(m=void 0,i++),f++}return!1})())())||(r.parent.indexOf("@!%")!==-1?t(r.dynFamily)(o=>()=>(e.units[r.id].main?e.units[o].endBeacon.parentNode.insertBefore(e.units[r.id].main,e.units[o].endBeacon):(e.units[o].endBeacon.parentNode.insertBefore(e.units[r.id].endBeacon,e.units[o].endBeacon),e.units[o].endBeacon.parentNode.insertBefore(e.units[r.id].startBeacon,e.units[r.id].endBeacon)),!0))()||(e.units[r.id].main?n.parentNode.replaceChild(e.units[r.id].main,n):(n.parentNode.replaceChild(e.units[r.id].endBeacon,n),e.units[r.id].endBeacon.parentNode.insertBefore(e.units[r.id].startBeacon,e.units[r.id].endBeacon))):t(r.dynFamily)(o=>()=>(e.units[r.id].startBeacon?(n.insertBefore(e.units[r.id].startBeacon,e.units[o].endBeacon),n.insertBefore(e.units[r.id].endBeacon,e.units[o].endBeacon)):n.insertBefore(e.units[r.id].main,e.units[o].endBeacon),!0))()||(e.units[r.id].startBeacon?(n.appendChild(e.units[r.id].startBeacon),n.appendChild(e.units[r.id].endBeacon)):n.appendChild(e.units[r.id].main)))}};var NC=t=>r=>e=>n=>()=>{var u,a,o=e.id;if(n.scopes[e.scope]||(n.scopes[e.scope]=[]),n.scopes[e.scope].push(o),!t(e.parent)(()=>()=>n.hydrating&&r&&(u=n.allBeacons[e.id])&&(a=n.allBeacons[`${e.id}%-%`])?(n.units[o]={listeners:{},parent:e.parent,scope:e.scope,pos:e.pos,dynFamily:e.dynFamily,startBeacon:u,endBeacon:a},u.$dekuId=o,a.$dekuId=o,!0):!1)()){let f=document.createComment(`%-%${e.id}`),m=document.createComment(`%-%${e.id}%-%`);n.units[o]={listeners:{},parent:e.parent,dynFamily:e.dynFamily,scope:e.scope,pos:e.pos,startBeacon:f,endBeacon:m},f.$dekuId=o,m.$dekuId=o}},fU=new Set(["animate","animateMotion","animateTransform","circle","clipPath","defs","desc","discard","ellipse","feBlend","feColorMatrix","feComponentTransfer","feComposite","feConvolveMatrix","feDiffuseLighting","feDisplacementMap","feDistantLight","feDropShadow","feFlood","feFuncA","feFuncB","feFuncG","feFuncR","feGaussianBlur","feImage","feMerge","feMergeNode","feMorphology","feOffset","fePointLight","feSpecularLighting","feSpotLight","feTile","feTurbulence","filter","foreignObject","g","image","line","linearGradient","marker","mask","metadata","mpath","path","pattern","polygon","polyline","radialGradient","rect","set","stop","svg","switch","symbol","text","textPath","title","tspan","use","view"]);var UC=t=>r=>()=>r.units[t]&&r.units[t].dynFamily?r.units[t].dynFamily:(()=>{throw new Error(`No positional information for ${t}`)})(),BC=t=>r=>()=>r.units[t]&&r.units[t].main&&r.units[t].main.parentNode&&r.units[t].main.parentNode.$dekuId?r.units[t].main.parentNode.$dekuId:r.units[t]&&r.units[t].startBeacon&&r.units[t].startBeacon.parentNode&&r.units[t].startBeacon.parentNode.$dekuId?r.units[t].startBeacon.parentNode.$dekuId:(()=>{throw new Error(`No parent information for ${t}`)})(),xA=t=>r=>()=>r.units[t]&&r.units[t].scope?r.units[t].scope:(()=>{throw new Error(`No scope information for ${t}`)})(),WC=t=>r=>e=>n=>()=>{var u,a=e.id;if(n.scopes[e.scope]||(n.scopes[e.scope]=[]),n.scopes[e.scope].push(a),!t(e.parent)(()=>()=>n.hydrating&&r&&(u=document.documentElement.querySelector(`[data-deku-ssr="${a}"]`))?(n.units[a]={listeners:{},pos:e.pos,parent:e.parent,scope:e.scope,dynFamily:e.dynFamily,main:u},u.$dekuId=a,!0):!1)()){let i=fU.has(e.tag)?document.createElementNS("http://www.w3.org/2000/svg",e.tag):document.createElement(e.tag);n.units[a]={listeners:{},parent:e.parent,pos:e.pos,scope:e.scope,dynFamily:e.dynFamily,main:i},i.$dekuId=a}},qC=t=>r=>e=>n=>u=>()=>{var a=n.id,o;if(u.scopes[n.scope]||(u.scopes[n.scope]=[]),u.scopes[n.scope].push(a),!t(n.parent)(f=>()=>{if(u.hydrating&&r&&(o=document.documentElement.querySelector(`[data-deku-ssr="${f}"]`))){for(var m=0;m<o.childNodes.length;m++){let A=a.split("@-@");if(o.childNodes[m].nodeType===8&&o.childNodes[m].nodeValue===A[0]){var v=m===0||o.childNodes[m-1].nodeType!==3;v&&m!==0?o.insertBefore(document.createTextNode(""),o.childNodes[m]):v?o.prepend(document.createTextNode("")):m=m-1;break}}let D=o.childNodes[m];return u.units[a]={main:D,pos:n.pos,parent:n.parent,scope:n.scope},D.$dekuId=a,!0}return!1})()){let f=document.createTextNode("");u.units[a]={main:f,parent:n.parent,scope:n.scope,pos:n.pos,dynFamily:n.dynFamily},f.$dekuId=a}};function CA(){return{units:{},scopes:{},allBeacons:{}}}var HC=t=>r=>e=>()=>{if(e.units[r.id]){var n=r.id,u=r.value;e.hydrating&&t&&!e.units[n]&&(dom=document.documentElement.querySelector(`[data-deku-ssr="${n}"]`))&&(e.units[n]={listeners:{},parent:r.parent,scope:r.scope,main:dom},e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(n)),e.units[n].main.tagName==="INPUT"&&r.key==="value"||e.units[n].main.tagName==="TEXTAREA"&&r.key==="value"?e.units[n].main.value=u:e.units[n].main.tagName==="INPUT"&&r.key==="checked"?e.units[n].main.checked=u==="true":r.key==="disabled"?e.units[n].main.disabled=u==="true":e.units[n].main.setAttribute(r.key,u)}},zC=t=>r=>e=>()=>{if(e.units[r.id]){var n=r.id,u=r.value;if(e.hydrating&&t&&!e.units[n]&&(dom=document.documentElement.querySelector(`[data-deku-ssr="${n}"]`))&&(e.units[n]={listeners:{},parent:r.parent,scope:r.scope,main:dom},e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(n)),r.key==="@self@")u(e.units[n].main)();else{e.units[n].listeners[r.key]&&e.units[n].main.removeEventListener(r.key,e.units[n].listeners[r.key]);var a=o=>u(o)();e.units[n].main.addEventListener(r.key,a),e.units[n].listeners[r.key]=a}}},VC=t=>r=>e=>()=>{if(e.units[r.id]){var n=r.id;e.hydrating&&t&&!e.units[n]&&(dom=document.documentElement.querySelector(`[data-deku-ssr="${n}"]`))&&(e.units[n]={listeners:{},parent:r.parent,scope:r.scope,main:dom},e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(n)),e.units[n].main.removeAttribute(r.key)}},GC=t=>r=>()=>{if(r.units[t.id]){var e=t.id;r.units[e].main.nodeValue=t.text}},JC=t=>r=>e=>n=>u=>()=>{var a,o,i=n.id,f=n.html,m=n.verb,v=n.cache,D=n.parent,A=n.scope,b=n.pxScope;let _=t(n.parent)(()=>()=>u.hydrating&&r&&(a=document.documentElement.querySelector(`[data-deku-ssr="${i}"]`))?(u.units[i]={listeners:{},pos:n.pos,scope:A,parent:D,main:a},a.$dekuId=i,!0):!1)();if(!_){let V=Object.entries(v);for(var k=0;k<V.length;k++){let et=V[k][0];V[k][1]===!0?f=f.replace(m+et+m,'data-deku-attr-internal="'+et+'"'):f=f.replace(m+et+m,'<span style="display:contents;" data-deku-elt-internal="'+et+'"></span>')}o=document.createElement("div"),o.innerHTML=f.trim(),u.units[i]={listeners:{},pos:n.pos,scope:A,parent:D,main:o.firstChild},o.firstChild.$dekuId=i}u.scopes[A]||(u.scopes[A]=[]),u.scopes[A].push(i),o||(o=a),o.querySelectorAll("[data-deku-attr-internal]").forEach(function(V){var et=V.getAttribute("data-deku-attr-internal");let Dt=et+"@!%"+b;u.units[Dt]={listeners:{},main:V,scope:A},u.scopes[A].push(Dt)}),o.querySelectorAll("[data-deku-elt-internal]").forEach(function(V){var et=V.getAttribute("data-deku-elt-internal");let Dt=et+"@!%"+b;u.units[et+"@!%"+b]={listeners:{},main:V,scope:A},u.scopes[A].push(Dt)}),_||u.units[i].main.remove()},jC=t=>r=>()=>{var e=t.id;r.units[e]={main:t.root},t.root.$dekuId=e},$A=t=>r=>e=>n=>()=>{let u=(_,k,V)=>{if(n.units[_].startBeacon){var et=n.units[_].startBeacon,Dt=et.nextSibling;for(n.units[k].main.insertBefore(et,V),et=Dt;et&&et!==n.units[_].endBeacon;)Dt=et.nextSibling,n.units[k].main.insertBefore(et,V),et=Dt}else n.units[k].main.insertBefore(n.units[_].main,V)},a=[];a.push(e);for(var o=0;o<a.length;o++){let _=a[o],k=_.id,V=_.parent;n.units[k].containingScope=_.scope;var i=void 0;r(_.pos)(Dt=>()=>(i=Dt,!0))(),i===void 0&&(i=Number.MAX_VALUE);let et=n.units[V].main.childNodes;for(var f=0,m=!1,v=0;f<et.length;){var D;if(D=et[f].$dekuId){if(r(_.dynFamily)(vr=>()=>m?!1:n.units[D].endBeacon===et[f]&&vr===D?(n.units[k].pos=t(v),u(k,V,et[f]),!0):!1)()){m=!0;break}if(n.units[D].dynFamily!==n.units[k].dynFamily){f++;continue}if(m){f++;continue}v===i?(u(k,V,et[f]),v++,m=!0):n.units[D].endBeacon!==et[f]&&(n.units[D].pos=t(v),v++)}f++}if(m)return;if(n.units[k].main)n.units[V].main.appendChild(n.units[k].main);else{var A=n.units[k].startBeacon,b=A.nextSibling;for(n.units[V].main.appendChild(A),A=b;A&&A!==n.units[k].endBeacon;)b=A.nextSibling,n.units[V].main.appendChild(A),A=b}}},XC=t=>r=>()=>{if(r.units[t.id]){var e=t.id;if(r.units[e].containingScope&&!t.scopeEq(r.units[e].containingScope)(t.scope))return;if(r.units[e].main)r.units[e].main.remove();else{let a=document.createElement("div");var n=r.units[e].startBeacon,u=n.nextSibling;for(a.appendChild(n),n=u;n&&n!==r.units[e].endBeacon;)u=n.nextSibling,a.appendChild(n),n=u;n===r.units[e].endBeacon&&a.appendChild(n)}}},QC=t=>r=>()=>r.units[t]!==void 0,TA=t=>r=>()=>{r.units[t.id]&&delete r.units[t.id]},KC=TA;function YC(t,r){return r.includes(t)}var Gut=typeof Array.from=="function",Jut=typeof Symbol<"u"&&Symbol!=null&&typeof Symbol.iterator<"u"&&typeof String.prototype[Symbol.iterator]=="function",jut=typeof String.prototype.fromCodePoint=="function",Xut=typeof String.prototype.codePointAt=="function";var r$=function(t){return function(r){return function(e){return(e|0)===e?t(e):r}}},Br=function(t){return t};var FA=function(t){return function(r){return Math.pow(t,r)|0}};var Is=isFinite;var A_=Math.floor;var vf=function(t){return function(r){return Math.pow(t,r)}},k_=function(t){return function(r){return t%r}},Ps=Math.round;var Ls=Math.sin;var sf=3.141592653589793;var n$=Ln(vp),u$=Rn(vp);var Al=function(){return r$($.create)(w.value)}(),a$=function(t){if(!Is(t))return 0;if(t>=Br(n$))return n$;if(t<=Br(u$))return u$;if(Jr)return Sn(0)(Al(t));throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): "+[t.constructor.name])},i$=function(t){return a$(Ps(t))};var d_=function(t){return a$(A_(t))};var b$=function(t){return function(r){return YC(t,r)}};var Ui=Math.random;var y_=function(t){return function(r){return function(){var n=Ui(),u=(Br(r)-Br(t)+1)*n+Br(t);return d_(u)}}};var LU=hu(Ga),RU=xn();var A$=function(t){return t};var NU=1,Bs=2147483647,UU=function(){return Bs-1|0}(),Df=function(t){var r=function(e){return function(n){return function(u){var a=n-e|0,o=LU(u)(a),i=o<e;return i?o+n|0:o}}};return r(NU)(UU)(t)};var BU=0,WU=48271,k$=function(t){return function(r){return RU(Al(k_(Br(WU)*Br(r)+Br(t))(Br(Bs))))}},d$=k$(BU);var Hs=function(t){var r=g(t);return{map:function(e){return function(n){return function(u){return r(function(a){return new N(e(a.value0),a.value1)})(n(u))}}}}};var NA=function(t){return{Applicative0:function(){return Gs(t)},Bind1:function(){return zs(t)}}},zs=function(t){var r=ct(t.Bind1());return{bind:function(e){return function(n){return function(u){return r(e(u))(function(a){var o=n(a.value0);return o(a.value1)})}}},Apply0:function(){return Vs(t)}}},Vs=function(t){var r=Hs(t.Bind1().Apply0().Functor0());return{apply:Gn(NA(t)),Functor0:function(){return r}}},Gs=function(t){var r=F(t.Applicative0());return{pure:function(e){return function(n){return r(new N(e,n))}},Apply0:function(){return Vs(t)}}};var UA=function(t){var r=F(t.Applicative0()),e=NA(t);return{state:function(n){return function(u){return r(n(u))}},Monad0:function(){return e}}};var S$=function(t){return function(r){var e=t(r);return e.value0}};var oB=UA(_a),fB=$o(oB);var cB=zs(_a);var lB=Hs(Va);var pB=Ye(Jn);var _B=Er(Jn);var vB=tx();var sB=function(t){return t};var $$=function(){var t=function(r){return new N(A$(r.newSeed),function(){var e={};for(var n in r)({}).hasOwnProperty.call(r,n)&&(e[n]=r[n]);return e.newSeed=d$(r.newSeed),e}())};return fB(t)}();var x_=lB,S_=g(x_),T$=S_(function(t){return Br(t)/Br(Bs)})($$);var bf=function(t){return S$(sB(t))};var Js=cB,mB=ct(Js);var js=Vs(_a),DB=at(js),C$=function(t){return function(r){var e=Br(r),n=Br(t),u=function(i){return n+k_(i)(e-n+1)},a=S_(Br)($$),o=DB(S_(_B)(a))(S_(pB(2))(a));return S_(function(i){return d_(u(i))})(o)}},WA=function(t){return function(r){var e=t<=r;return e?C$(t)(r):C$(r)(t)}};var C_=Gs(_a),bB=F(C_);var qA=function(t){return mB(WA(0)(ZS(t)-1|0))(function(r){return bB(vB(t)(r))})};var sc=function(t){return t.arbitrary};var F$={arbitrary:T$};var Xs=function(){return{arbitrary:WA(-1e6)(1e6)}}();var E$=g(ft),hB=g(Qe),SB=g(L),xB=Ir(Kn);var CB=Ft(gu),$B=sc(Xs),TB=Er(pa);var Ks=F(Bt),FB=Nt(Kf(lh(uo))),EB=h(Dr),M$=tn(),MB=ie(),OB=dt(go),wB=qu(ws),eu=F(j);var Ou=function(t){return t},O$={map:function(t){return function(r){return E$(hB(SB(t)))(r)}}};var mc=function(t){return function(r){return t instanceof $?r(t.value0):Ks(!1)}},IB=function(t){return function(r){return function(){var n=xA(t.id)(r)(),u=BC(t.id)(r)(),a=UC(t.id)(r)(),o={scope:n,parent:u,dynFamily:a,id:t.id,pos:new $(t.pos),ez:!1,raiseId:FB,ctor:tl(EB)};return M$($A($.create)(mc)(o))(r)()}}},PB=function(t){return function(r){return function(e){return Zn({doLogic:function(n){return function(u){return function(a){return u.sendToPos({id:a,pos:n})}}},ids:function(n){return function(u){return u.ids}(MB(n))},disconnectElement:function(n){return function(u){return n.disconnectElement({id:u.id,scope:u.scope,parent:u.parent,scopeEq:OB})}},toElt:function(n){return n}})(t)(r)(M$(e))}}},LB=function(t){return function(r){return function(e){return function(n){return wB(Hn(eu(function(u){var a=function(){var i=E$(function(f){return function(m){return Ks(f)}})(PB({dynFamily:n.dynFamily,ez:n.ez,parent:new $(n.parent),pos:n.pos,raiseId:n.raiseId,scope:n.scope})(t)(n.ctor));return Ks(IC(i))}(),o=Ks(Hn(eu($A(r)(e)(n))));return function(){var f=QC(n.id)(u)();if(f){var m=xA(n.id)(u)();if(m instanceof lf)return o();if(m instanceof Eu&&n.scope instanceof Eu){var v=b$(m.value0)(n.scope.value0);return v?o():a()}return a()}return a()}})))}}}},w$=function(t){var r={ids:function(){var n=Ze(t)(),u=CB(bf($B)({newSeed:Df(n),size:5}));return xB(ja(TB(1))(t))(),u},makeElement:function(){var e=WC(mc)(!1);return function(n){return Hn(Ou(eu(e(n))))}}(),makeDynBeacon:function(){var e=NC(mc)(!1);return function(n){return Hn(Ou(eu(e(n))))}}(),attributeParent:function(){var e=RC(mc);return function(n){return Hn(Ou(eu(e(n))))}}(),makeRoot:function(e){return Hn(Ou(eu(jC(e))))},makeText:function(){var e=qC(mc)(!1)(yt(void 0));return function(n){return Hn(Ou(eu(e(n))))}}(),makePursx:function(){var e=JC(mc)(!1)(yt(void 0));return function(n){return Hn(Ou(eu(e(n))))}}(),setProp:function(){var e=HC(!1);return function(n){return Hn(Ou(eu(e(n))))}}(),setCb:function(){var e=zC(!1);return function(n){return Hn(Ou(eu(e(n))))}}(),unsetAttribute:function(){var e=VC(!1);return function(n){return Hn(Ou(eu(e(n))))}}(),setText:function(e){return Hn(Ou(eu(GC(e))))},sendToPos:function(e){return Hn(Ou(eu(IB(e))))},removeDynBeacon:function(e){return Hn(Ou(eu(KC(e))))},deleteFromCache:function(e){return Hn(Ou(eu(TA(e))))},giveNewParent:function(e){return LB(r)($.create)(mc)(e)},disconnectElement:function(e){return Hn(Ou(eu(XC(e))))}};return r};var Fo=function(){return window};function P$(t,r,e,n){if(typeof window<"u"){var u=window[e];if(u!=null&&n instanceof u)return r(n)}for(var a=n;a!=null;){var o=Object.getPrototypeOf(a),i=o.constructor.name;if(i===e)return r(n);if(i==="Object")return t;a=o}return t}var At=function(t){return function(r){return P$(w.value,$.create,t,r)}};function L$(t,r,e){return t==null?r:e(t)}var We=function(t){return L$(t,w.value,$.create)};var zA=At("HTMLCanvasElement");function U$(t){return t.body}var XB=g(L);var B$=function(t){return XB(We)(function(){return U$(t)})};var W$=J;function Dc(t){return function(){return t.valueAsNumber}}var yl=At("HTMLInputElement");function GA(t){return function(){return t.document}}function Ys(t){return function(r){return function(){return r.requestAnimationFrame(t)}}}var JA=J;var z$=ka(me),V$=g(ft),C2=wC(O$),G$=io(uo),$2=Nt(ES(Kf(G$))),T2=F(Bt),jA=ct(Qn),F2=oe(L),E2=Aa(ba),M2=Nt(io(G$)),O2=g(Ae),w2=Ir(L);var I2=function(t){var r=function(u){var a=V$(e(u));return function(o){return z$(a(o))}},e=function(u){return function(a){return function(o){if(o instanceof St)return z$(V$(n(u))(o.value0));if(o instanceof xt)return $2;throw new Error("Failed pattern match at Deku.Toplevel (line 47, column 21 - line 49, column 22): "+[o.constructor.name])}(C2(a))}},n=function(u){return function(a){return r(u+1|0)(Cn(function(o){return function(){return void 0,jA(a(t))(o)(),T2(void 0)}}))}};return r(0)},P2=function(t){return function(r){return function(){var n=CA(),u=F2(E2(Fe(0)))(function(){var a=Bx(t)(r);return function(o){return a(w$(o))}}())();return we(I2(n)(u))(function(a){return a(n)})()}}},L2=function(t){return function(){var e=jA(jA(Fo)(GA))(B$)();return yt(M2)(function(n){return P2(n)(t)})(O2(W$)(e))()}},J$=function(t){return w2(L2(t))};var XA=h(Dr),N2=cf(qt),j$=F(j),U2=F(Ee),B2={reflectType:function(){return"~"}},X$=gt(Wt),W2=g(ft),q2=Ju(Wn),H2=ie(),z2=dt(go),V2=tn();var R={pursxToElement:function(t){return function(r){return function(e){return{cache:wi,element:new _r(function(n){return function(u){return XA}})}}}}},QA=function(t){return t.pursxToElement};var KA=function(t){var r=function(e){return e instanceof _r?e.value0:function(n){return function(u){return XA}}};return r(t)},Yt=function(t){var r=pc(t);return function(e){var n=pc(e);return function(){return function(){return function(u){var a=QA(u);return function(o){return function(i){return function(f){var m=function(D){return function(A){return mr(function(b,_){var k=A.ids(),V=A.ids();D.raiseId(k)();var et=a(V)(d.value)(f),Dt=KA(et.element),vr=b(N2([j$(A.makePursx({id:k,parent:D.parent,cache:et.cache,dynFamily:D.dynFamily,pos:D.pos,pxScope:V,scope:D.scope,html:r(i),verb:n(o)})),Dt(D)(A),yt(XA)(function(Gr){return j$(A.attributeParent({id:k,parent:Gr,pos:D.pos,dynFamily:D.dynFamily,ez:!1}))})(D.parent)]),_);return function(){return _(A.deleteFromCache({id:k})),vr()}})}},v=new _r(m);return v}}}}}}}},mt=function(t){var r=Yt(t)(B2)()();return function(){return function(){return function(e){return r(e)(d.value)}}}};var Je=function(){return function(t){var r=QA(t);return function(e){var n=pc(e);return function(u){var a=ko(u)();return{pursxToElement:function(o){return function(i){return function(f){var m=r(o)(d.value)(f);return{cache:So(n(d.value))(!0)(m.cache),element:new _r(function(v){return function(D){return X$(W2(q2(as)(function(A){if(A.value instanceof jp)return D.setProp({id:n(d.value)+("@!%"+o),key:A.key,value:A.value.value0});if(A.value instanceof Xp)return D.setCb({id:n(d.value)+("@!%"+o),key:A.key,value:A.value.value0});if(A.value instanceof us)return D.unsetAttribute({id:n(d.value)+("@!%"+o),key:A.key});throw new Error("Failed pattern match at Deku.Pursx (line 2487, column 52 - line 2504, column 38): "+[A.value.constructor.name])}))(a(d.value)(f)))(function(){var A=KA(m.element);return A}()(v)(D))}})}}}}}}}}};var G2=function(t){return function(r){return function(e){return Zn({doLogic:function(n){return function(u){return function(a){return u.sendToPos({id:a,pos:n})}}},ids:function(n){return function(u){return u.ids}(H2(n))},disconnectElement:function(n){return function(u){return n.disconnectElement({id:u.id,scope:u.scope,parent:u.parent,scopeEq:z2})}},toElt:function(n){return n}})(t)(r)(V2(e))}}},W=function(){return function(t){var r=QA(t);return function(e){var n=pc(e);return function(u){var a=ko(u)();return{pursxToElement:function(o){return function(i){return function(f){var m=r(o)(d.value)(f);return{cache:So(n(d.value))(!1)(m.cache),element:new _r(function(v){return function(D){return X$(G2({parent:new $(n(d.value)+("@!%"+o)),scope:v.scope,raiseId:function(A){return U2(void 0)},pos:v.pos,ez:!1,dynFamily:w.value})(D)(function(A){return A}(a(d.value)(f))))(function(){var A=KA(m.element);return A}()(v)(D))}})}}}}}}}}};var J2=LS()(),j2=Q(tA),X2=fa(a_),Q2=i_()(),Sr=function(){return function(){return{defaults:Gt(J2)}}},K2=function(t){return t.defaults},xr={convertRecordOptions:function(t){return function(r){return function(e){return j2}}}},Q$=function(t){return t.convertRecordOptions},zn=function(t){return t.convertOptionsWithDefaults},Cr=function(){return function(t){var r=Q$(t);return{convertOptions:function(e){return function(n){return Ax(r(e)(d.value)(n))}}}}},Y2=function(t){return t.convertOptions},Zt=function(t){var r=Y2(t);return function(e){var n=K2(e);return{convertOptionsWithDefaults:function(u){return function(a){var o=n(a),i=r(u);return function(f){return o(i(f))}}}}}},Z2=function(t){return t.convertOption},kt=function(t){var r=Q$(t);return function(e){var n=Z2(e);return function(){return function(){return function(){return function(u){var a=Q2(u),o=ko(u)();return{convertRecordOptions:function(i){return function(f){return function(m){return X2(a(d.value)(n(i)(d.value)(o(d.value)(m))))(r(i)(d.value)(m))}}}}}}}}}};var r3=function(){return function(){return function(){return function(t){return function(r){return function(e){return Cv(e.type)(t)?Au(e.type)(t)(e.value):r(e)}}}}}},e3=r3()()();var Eo=function(){return function(t){var r=fe(t);return function(e){return function(n){return{type:r(e),value:n}}}}};var n3=function(t){return Fu("Data.Variant: pattern match failure ["+(t.type+"]"))},YA=function(){return function(){return function(){return function(t){return e3(t)(n3)}}}};var ZA=function(){var t=Hv(Ab);return function(r){return zv(t(r))}}();var Yu=void 0;var um=function(t){return t.toInt},e0=function(t){var r=um(t);return function(e){return r(Yu)}};var Zu={toInt:function(t){return 8}},n0={Nat0:function(){return Zu}},Bi={toInt:function(t){return 7}},u0={Nat0:function(){return Bi}},Wi={toInt:function(t){return 6}},a0={Nat0:function(){return Wi}},uu={toInt:function(t){return 5}},i0={Nat0:function(){return uu}},Ta={toInt:function(t){return 4}},fi={Nat0:function(){return Ta}},Fa={toInt:function(t){return 3}},Mo={Nat0:function(){return Fa}},Ea={toInt:function(t){return 2}},Oo={Nat0:function(){return Ea}},Ma={toInt:function(t){return 1}},wo={Nat0:function(){return Ma}},vn={toInt:function(t){return 0}};var Se=function(t){return function(){return function(r){var e=r.Nat1();return function(){return function(n){return{Nat0:function(){return e},Pos1:function(){return t}}}}}}};var ci={Nat0:function(){return Bi},Nat1:function(){return Zu}};var li={Nat0:function(){return Wi},Nat1:function(){return Zu}};var pi={Nat0:function(){return uu},Nat1:function(){return Zu}};var _i={Nat0:function(){return Ta},Nat1:function(){return Zu}};var qi={Nat0:function(){return Ta},Nat1:function(){return uu}};var vi={Nat0:function(){return Fa},Nat1:function(){return Zu}};var Hi={Nat0:function(){return Fa},Nat1:function(){return uu}};var si={Nat0:function(){return Ea},Nat1:function(){return Zu}};var zi={Nat0:function(){return Ea},Nat1:function(){return uu}};var mi={Nat0:function(){return Ma},Nat1:function(){return Zu}};var Vi={Nat0:function(){return Ma},Nat1:function(){return uu}};var Di={Nat0:function(){return vn},Nat1:function(){return Zu}};var Gi={Nat0:function(){return vn},Nat1:function(){return uu}};var o0={Nat0:function(){return vn},Nat1:function(){return Zu}};var tk={Nat0:function(){return vn},Nat1:function(){return Bi}};var rk={Nat0:function(){return vn},Nat1:function(){return Wi}};var ek={Nat0:function(){return vn},Nat1:function(){return uu}};var Io={Nat0:function(){return vn},Nat1:function(){return Ta}};var ta={Nat0:function(){return vn},Nat1:function(){return Fa}};var ra={Nat0:function(){return vn},Nat1:function(){return Ea}};var ea={Nat0:function(){return vn},Nat1:function(){return Ma}},Po={Nat0:function(){return vn},Nat1:function(){return vn}};var y3=zc(),g3=g(ir);var f0=xi;var am=function(t){return t};var nk=function(t){var r=um(t);return function(){return function(e){return function(n){return y3(e)(r(n))}}}};var uk=function(t){var r=e0(t);return function(e){var n=r(d.value),u=function(){return n===0?[]:Ke(0)(n-1|0)}();return g3(e)(u)}};var Oa=[];var Ie=function(t){return function(r){return function(e){return tf(r)(e)}}};var Af={first:function(t){return function(r){return new N(t(r.value0),r.value1)}},second:g(lo),Profunctor0:function(){return Wn}},xl=function(t){return t.second},im=function(t){return t.first};var p0=tn();var I3=function(t){return function(r){return function(e){var n=Ii(e);return function(u){return n(t)(r)(u)}}}};var _0=function(){return function(){return function(t){return I3(p0)(p0)(t)}}};var L3=_0()(),v0=function(){return function(){return function(t){return L3(t)}}};var N3=function(t){return function(r){var e=Ii(r.Profunctor0()),n=im(r);return function(u){return e(t)(function(a){return a.value1(a.value0)})(n(u))}}},s0=function(t){return function(r){return function(e){return N3(function(n){return new N(t(n),function(u){return r(n)(u)})})(e)}}};var m0=function(t){var r=ko(t)(),e=PS(t)()();return function(){return function(){return function(n){return function(u){return s0(r(n))(Gt(e(n)))(u)}}}}};var D0=function(t){return t};var z3=JSON.parse;var V3=JSON.stringify;var X3=ie();var ik=function(t){return X3(Cs(t))};function T_(t){return Object.prototype.toString.call(t).slice(8,-1)}var k0=Array.isArray||function(t){return Object.prototype.toString.call(t)==="[object Array]"};var ck=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}();var d0=J;var lk=function(t){var r=ui(uC(t));return function(e){return r(ZA(e))}};var pk=function(t){var r=F(s_(t)),e=lk(t);return function(n){return function(u){if(T_(u)===n)return r(d0(u));if(Jr)return e(new ck(n,T_(u)));throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): "+[n.constructor.name,u.constructor.name])}}};var _k=function(t){return pk(t)("String")};var lW=m0({reflectSymbol:function(){return"o"}})()();var pW={reflectSymbol:function(){return"2x"}};var _W={reflectSymbol:function(){return"on"}},vW={reflectSymbol:function(){return"off"}};var sW=v0()()(Wn);var wa=Eo(),mW=F(j),DW=po(),bW=wa({reflectSymbol:function(){return"onOff"}});var om=function(){function t(){}return t.value=new t,t}(),fm=function(){function t(){}return t.value=new t,t}(),g0=function(){function t(){}return t.value=new t,t}(),h0=function(){function t(){}return t.value=new t,t}(),sk=function(){function t(){}return t.value=new t,t}(),S0=function(){function t(){}return t.value=new t,t}(),x0=function(){function t(){}return t.value=new t,t}();var C0=function(t){return t},$0=function(t){return t};var T0=function(t){return t};var F0=function(t){return t};var E0=function(t){return t};var M0=function(t){return t},O0=function(t){return t},w0=function(t){return t},I0=function(t){return t},P0=function(t){return t};var mk=function(){function t(){}return t.value=new t,t}(),L0=function(){function t(){}return t.value=new t,t}(),R0=function(){function t(){}return t.value=new t,t}(),Dk=function(){function t(){}return t.value=new t,t}(),N0=function(){function t(){}return t.value=new t,t}();var cm=function(t){return t};var Cl=function(t){return t};var AW=function(t){return t},F_=function(t){return t};var kc={toAudioOnOff:Q(Z)};var dc=function(t){return t.toAudioParameter},U0=function(t){return t.toAudioOnOff},B0=function(){return Yc.create}(),W0=function(){return Zc.value}();var lm=function(){return D0(function(){var t=lW(d.value)(Af);return function(r){return sW(t(r))}}())},q0=J;var kW=function(){var t=wa({reflectSymbol:function(){return"unit"}})(d.value);return function(r){return F_(t(r))}}();var dW=function(t){return{toAudioParameter:function(r){return kW(r)}}},H0=function(t){return{toAudioParameter:function(){var r=dc(dW(t));return function(e){return r(AW(function(n){return{u:n}}(e)))}}()}},z0=function(){return wa(pW)(d.value)(void 0)}(),V0=function(){var t=wa({reflectSymbol:function(){return"sudden"}})(d.value);return function(r){return F_(t(r))}}();var G0={toAudioParameter:V0},pm={toAudioParameter:function(t){return V0({n:t})}},bk=function(){return wa({reflectSymbol:function(){return"step"}})(d.value)(void 0)}();var Ak=function(){return wa(_W)(d.value)(void 0)}(),E_={x:Ak,o:0},X=function(){return mW(DW(bW(d.value)(E_)))};var J0=function(){return wa(vW)(d.value)(void 0)}();var yW=function(){var t=wa({reflectSymbol:function(){return"numeric"}})(d.value);return function(r){return F_(t(r))}}();var Ia={toAudioParameter:yW};var Ji=function(){return wa({reflectSymbol:function(){return"linear"}})(d.value)(void 0)}();var j0=function(){return wa({reflectSymbol:function(){return"exponential"}})(d.value)(void 0)}(),gW=function(){var t=wa({reflectSymbol:function(){return"envelope"}})(d.value);return function(r){return F_(t(r))}}();var En={toAudioParameter:gW},hW=function(){var t=wa({reflectSymbol:function(){return"cancel"}})(d.value);return function(r){return F_(t(r))}}();var X0={toAudioParameter:hW};var SW=Eo(),xW=SW({reflectSymbol:function(){return"realImg"}}),ce=Q(Z),Q0=rx(lc),K0=tn();var kf=Cr(),df=kt(xr),yf=Sr()(),Y0={reflectSymbol:function(){return"buffer"}},_m={reflectSymbol:function(){return"frequency"}},CW=function(){function t(){}return t.value=new t,t}(),$W=function(){function t(){}return t.value=new t,t}(),TW=function(){function t(){}return t.value=new t,t}(),FW=function(){function t(){}return t.value=new t,t}(),EW=function(){function t(){}return t.value=new t,t}(),MW=function(){function t(){}return t.value=new t,t}(),OW=function(){function t(){}return t.value=new t,t}(),wW=function(){function t(){}return t.value=new t,t}(),IW=function(){function t(){}return t.value=new t,t}(),PW=function(){function t(){}return t.value=new t,t}(),LW=function(){function t(){}return t.value=new t,t}(),RW=function(){function t(){}return t.value=new t,t}(),NW=function(){function t(){}return t.value=new t,t}(),UW=function(){function t(){}return t.value=new t,t}(),$l=function(t){return{toPeriodicOscSpec:function(r){return xW(d.value)({real:am(r.value0),img:am(r.value1)})}}};var vm={toInitializeTriangleOsc:function(t){return P0(function(r){return{frequency:r}}(t))}};var Z0={toInitializeStereoPanner:function(t){return I0(function(r){return{pan:r}}(t))}};var Tl={toInitializeSquareOsc:function(t){return w0(function(r){return{frequency:r}}(t))}};var gf={toInitializeSinOsc:function(t){return O0(function(r){return{frequency:r}}(t))}};var tT={toInitializeSawtoothOsc:function(t){return M0(function(r){return{frequency:r}}(t))}};var kk={toInitializeRecorder:function(t){return C0(function(r){return{cb:r}}(t))}};var M_={toInitializeMicrophone:function(t){return $0(function(r){return{microphone:r}}(t))}};var rT=function(t){return function(r){return{toInitializeIIRFilter:function(e){return function(n){return function(u){return{feedforward:Q0(K0(e.value0)),feedback:Q0(K0(e.value1))}}}}}}};var pt={toInitializeGain:function(t){return E0(function(r){return{gain:r}}(t))}};var eT={toInitializeConvolver:function(t){return T0(function(r){return{buffer:r}}(t))}},sm={toInitializeConstant:function(t){return F0(function(r){return{offset:r}}(t))}};var BW={convertOption:function(t){return function(r){return ce}}},O_={convertOption:function(t){return function(r){return ce}}},nT={convertOption:function(t){return function(r){return ce}}},uT={convertOption:function(t){return function(r){return $.create}}},aT={convertOption:function(t){return function(r){return ce}}},Fl={convertOption:function(t){return function(r){return ce}}},iT={convertOption:function(t){return function(r){return ce}}},oT={convertOption:function(t){return function(r){return ce}}},fT={convertOption:function(t){return function(r){return ce}}},cT={convertOption:function(t){return function(r){return ce}}},lT={convertOption:function(t){return function(r){return ce}}},pT={convertOption:function(t){return function(r){return ce}}},_T={convertOption:function(t){return function(r){return ce}}},vT={convertOption:function(t){return function(r){return ce}}},dk={convertOption:function(t){return function(r){return ce}}},El={convertOption:function(t){return function(r){return ce}}},mm={convertOption:function(t){return function(r){return ce}}},Dm={convertOption:function(t){return function(r){return ce}}};var bm={convertOption:function(t){return function(r){return ce}}},sT={convertOption:function(t){return function(r){return ce}}},mT={convertOption:function(t){return function(r){return ce}}},DT={convertOption:function(t){return function(r){return ce}}},yk={convertOption:function(t){return function(r){return ce}}};var bT={convertOption:function(t){return function(r){return ce}}},gk={convertOption:function(t){return function(r){return ce}}},ji={convertOption:function(t){return function(r){return ce}}},bi={convertOption:function(t){return function(r){return ce}}},AT={convertOption:function(t){return function(r){return ce}}},hk={convertOption:function(t){return function(r){return ce}}},WW=function(t){return t.toPeriodicOscSpec},Ml=function(t){var r=WW(t);return{convertOption:function(e){return function(n){return r}}}},Sk=function(t){return t.toInitializeWaveShaper},kT=function(t){return t.toInitializeTriangleOsc},dT=function(t){return t.toInitializeStereoPanner},yT=function(t){return t.toInitializeSquareOsc},gT=function(t){return t.toInitializeSinOsc},hT=function(t){return t.toInitializeSawtoothOsc},ST=function(t){return t.toInitializeRecorder},xk=function(t){return t.toInitializePlayBuf},xT=function(t){return t.toInitializePeriodicOsc},CT=function(t){return t.toInitializePeaking},$T=function(t){return t.toInitializeNotch},TT=function(t){return t.toInitializeMicrophone},FT=function(t){return t.toInitializeLowshelf},Ck=function(t){return t.toInitializeLowpass},$k=function(t){return t.toInitializeLoopBuf},ET=function(t){return t.toInitializeIIRFilter},MT=function(t){return t.toInitializeHighshelf},Tk=function(t){return t.toInitializeHighpass},OT=function(t){return t.toInitializeGain},wT=function(t){return t.toInitializeDynamicsCompressor},Fk=function(t){return t.toInitializeDelay},IT=function(t){return t.toInitializeConvolver},PT=function(t){return t.toInitializeConstant},Ek=function(t){return t.toInitializeBandpass},Mk=function(t){return t.toInitializeAllpass};var qW={oversample:z0},HW=function(t){var r=zn(t);return{toInitializeWaveShaper:function(e){return r(CW.value)(qW)(e)}}},LT={toInitializeWaveShaper:function(){var t=Sk(HW(Zt(kf(df(BW)()()()({reflectSymbol:function(){return"curve"}})))(yf)));return function(r){return t(function(e){return{curve:e}}(r))}}()},zW=function(){return{bufferOffset:0,playbackRate:1,duration:w.value}}(),w_=function(t){var r=zn(t);return{toInitializePlayBuf:function(e){return r($W.value)(zW)(e)}}},Pa={toInitializePlayBuf:function(){var t=xk(w_(Zt(kf(df(O_)()()()(Y0)))(yf)));return function(r){return t(function(e){return{buffer:e}}(r))}}()},VW={},Ol=function(t){var r=zn(t);return{toInitializePeriodicOsc:function(e){return r(TW.value)(VW)(e)}}},GW={q:1,gain:0},RT=function(t){var r=zn(t);return{toInitializePeaking:function(e){return r(FW.value)(GW)(e)}}};var JW={q:1},NT=function(t){var r=zn(t);return{toInitializeNotch:function(e){return r(EW.value)(JW)(e)}}};var jW={gain:0},UT=function(t){var r=zn(t);return{toInitializeLowshelf:function(e){return r(MW.value)(jW)(e)}}};var XW={q:1},Ok=function(t){var r=zn(t);return{toInitializeLowpass:function(e){return r(OW.value)(XW)(e)}}},Am={toInitializeLowpass:function(){var t=Ck(Ok(Zt(kf(df(dk)()()()(_m)))(yf)));return function(r){return t(function(e){return{frequency:e}}(r))}}()},QW=function(){return{loopStart:0,loopEnd:0,playbackRate:1,duration:w.value}}(),wl=function(t){var r=zn(t);return{toInitializeLoopBuf:function(e){return r(wW.value)(QW)(e)}}},cr={toInitializeLoopBuf:function(){var t=$k(wl(Zt(kf(df(El)()()()(Y0)))(yf)));return function(r){return t(function(e){return{buffer:e}}(r))}}()},KW={gain:0},BT=function(t){var r=zn(t);return{toInitializeHighshelf:function(e){return r(IW.value)(KW)(e)}}};var YW={q:1},wk=function(t){var r=zn(t);return{toInitializeHighpass:function(e){return r(PW.value)(YW)(e)}}},Il={toInitializeHighpass:function(){var t=Tk(wk(Zt(kf(df(yk)()()()(_m)))(yf)));return function(r){return t(function(e){return{frequency:e}}(r))}}()},ZW=function(){return{ratio:12,attack:.003,release:.25,knee:30,threshold:-24}}(),WT=function(t){var r=zn(t);return{toInitializeDynamicsCompressor:function(e){return r(LW.value)(ZW)(e)}}},tq={maxDelayTime:1},Ik=function(t){var r=zn(t);return{toInitializeDelay:function(e){return r(RW.value)(tq)(e)}}},Ai={toInitializeDelay:function(){var t=Fk(Ik(Zt(kf(df(gk)()()()({reflectSymbol:function(){return"delayTime"}})))(yf)));return function(r){return t(function(e){return{delayTime:e}}(r))}}()},rq={q:1},ki=function(t){var r=zn(t);return{toInitializeBandpass:function(e){return r(NW.value)(rq)(e)}}},qT={toInitializeBandpass:function(){var t=Ek(ki(Zt(kf(df(bi)()()()(_m)))(yf)));return function(r){return t(function(e){return{frequency:e}}(r))}}()},eq={q:1},Pk=function(t){var r=zn(t);return{toInitializeAllpass:function(e){return r(UW.value)(eq)(e)}}},HT={toInitializeAllpass:function(){var t=Mk(Pk(Zt(kf(df(hk)()()()(_m)))(yf)));return function(r){return t(function(e){return{frequency:e}}(r))}}()};var aq=function(t,r,e){var n=0,u;return function(a){if(n===2)return u;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+a+")",r,a);return n=1,u=e(),n=2,u}},GT=Q(Z);var km=ie(),P_=tn();var zT=F(Bt),br=F(j),An=F(Ee),xe=rn(qt)(Dr),an=g(ft),je=YA()()(),su=h(Dr),VT=ps(),I_=Eo(),iq=gt(Wt),oq=Ir(Kn),sn=ka(me),fq=aA()()(),JT=Ju(Wn),cq=g(e_),lq=Gb({reflectType:function(){return 0}})()()();var pq=function(){function t(){}return t.value=new t,t}();var dm={convertOption:function(t){return function(r){return GT}}},ym={convertOption:function(t){return function(r){return GT}}};var _q=function(t){return t.toInitializeAnalyser},ye=function(t){if(t instanceof lf)return w.value;if(t instanceof Eu)return new $(t.value0);throw new Error("Failed pattern match at Ocarina.Control (line 38, column 1 - line 38, column 38): "+[t.constructor.name])},wu=Cx({doLogic:Jo,ids:function(t){return function(r){return r.ids}(km(t))},disconnectElement:function(t){return function(r){return t.disconnectXFromY({from:r.id,to:r.parent})}},toElt:function(t){return t}})({fromElt:P_,connectToParent:function(t){return function(r){return t.connectXToY({from:r.id,to:r.parent})}}});var vq=function(){return{cb:function(t){return zT(zT(void 0))},fftSize:sk.value,maxDecibels:-30,minDecibels:-100,smoothingTimeConstant:.8,channelCount:2,channelCountMode:Dk.value,channelInterpretation:mk.value}}(),gm=function(t){var r=zn(t);return{toInitializeAnalyser:function(e){return r(pq.value)(vq)(e)}}};var sq=function(t){var r=TT(t);return function(e){var n=r(e),u=function(a){return function(o){return mr(function(i,f){var m=o.ids();a.raiseId(m)();var v=i(br(o.makeMicrophone({id:m,parent:a.parent,scope:ye(a.scope),microphone:n.microphone})),f);return function(){return f(o.deleteFromCache({id:m})),v()}})}};return new _r(u)}},L_=function(t){return sq(t)};var kn=Zn({doLogic:Jo,ids:function(t){return function(r){return r.ids}(km(t))},disconnectElement:function(t){return function(r){return t.disconnectXFromY({from:r.id,to:r.parent})}},toElt:function(t){return t}}),mq=function(t){var r=_q(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([br(f.makeAnalyser({id:D,parent:i.parent,scope:ye(i.scope),cb:a.cb,fftSize:FA(2)(function(){if(a.fftSize instanceof om)return 7;if(a.fftSize instanceof fm)return 8;if(a.fftSize instanceof g0)return 9;if(a.fftSize instanceof h0)return 10;if(a.fftSize instanceof sk)return 11;if(a.fftSize instanceof S0)return 12;if(a.fftSize instanceof x0)return 13;throw new Error("Failed pattern match at Ocarina.Control (line 198, column 27 - line 205, column 40): "+[a.fftSize.constructor.name])}()),maxDecibels:a.maxDecibels,minDecibels:a.minDecibels,smoothingTimeConstant:a.smoothingTimeConstant,channelCount:a.channelCount,channelCountMode:function(){if(a.channelCountMode instanceof N0)return"explicit";if(a.channelCountMode instanceof Dk)return"max";if(a.channelCountMode instanceof R0)return"clamped-max";throw new Error("Failed pattern match at Ocarina.Control (line 211, column 41 - line 214, column 52): "+[a.channelCountMode.constructor.name])}(),channelInterpretation:function(){if(a.channelInterpretation instanceof mk)return"speakers";if(a.channelInterpretation instanceof L0)return"discrete";throw new Error("Failed pattern match at Ocarina.Control (line 215, column 46 - line 217, column 47): "+[a.channelInterpretation.constructor.name])}()})),an(function(b){return je({cb:function(_){return f.setAnalyserNodeCb({id:D,cb:_})}})(b)})(n),kn({parent:new $(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},hm=function(t){var r=mq(t);return function(e){return r(e)(su)}},jT=function(t){var r=IT(t);return function(e){return function(n){var u=r(e),a=function(o){return function(i){return mr(function(f,m){var v=i.ids();o.raiseId(v)();var D=f(xe([br(i.makeConvolver({id:v,parent:o.parent,scope:ye(o.scope),buffer:u.buffer})),kn({parent:new $(v),scope:o.scope,raiseId:function(A){return An(void 0)}})(i)(ze(n))]),m);return function(){return m(i.deleteFromCache({id:v})),D()}})}};return new _r(a)}}},Dq=function(){return function(){return function(t){var r=ET(t);return function(e){return function(n){return function(u){return function(a){var o=r(u)(e)(n),i=function(f){return function(m){return mr(function(v,D){var A=m.ids();f.raiseId(A)();var b=v(xe([br(m.makeIIRFilter({id:A,parent:f.parent,scope:ye(f.scope),feedforward:VT(o.feedforward),feedback:VT(o.feedback)})),kn({parent:new $(A),scope:f.scope,raiseId:function(_){return An(void 0)}})(m)(ze(a))]),D);return function(){return D(m.deleteFromCache({id:A})),b()}})}};return new _r(i)}}}}}}},bq=Dq()(),XT=function(){return function(){return function(t){return bq(t)(d.value)(d.value)}}},Lk=function(t){var r=ST(t);return function(e){return function(n){var u=r(e),a=function(o){return function(i){return mr(function(f,m){var v=i.ids();o.raiseId(v)();var D=f(xe([br(i.makeRecorder({id:v,parent:o.parent,scope:ye(o.scope),cb:u.cb})),kn({parent:new $(v),scope:o.scope,raiseId:function(A){return An(void 0)}})(i)(n)]),m);return function(){return m(i.deleteFromCache({id:v})),D()}})}};return new _r(a)}}},Aq=function(t){return function(r){return mr(function(e,n){var u=r.ids();return n(r.makeSpeaker({id:u})),e(kn({parent:new $(u),scope:new Eu("toplevel"),raiseId:function(a){return An(void 0)}})(r)(ze(t)),n)})}},yc=Aq,wt=function(t){return function(r){return function(e){return mu(t)(r)(su)(e)}}},mu=function(t){var r=OT(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([br(f.makeGain({id:D,parent:i.parent,scope:ye(i.scope),gain:a.gain})),sn(an(function(b){return je({gain:QT(639)(i.scope)(f)(function(_){return f.setGain(function(k){return{id:D,gain:k}}(_))})})(b)})(n)),kn({parent:new $(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},QT=aq("tmpResolveAU","Ocarina.Control",function(){var t=function(){var o=I_({reflectSymbol:function(){return"unit"}})(d.value);return function(i){return Cl(o(i))}}(),r=function(){var o=I_({reflectSymbol:function(){return"sudden"}})(d.value);return function(i){return Cl(o(i))}}(),e=function(){var o=I_({reflectSymbol:function(){return"numeric"}})(d.value);return function(i){return Cl(o(i))}}(),n=function(){var o=I_({reflectSymbol:function(){return"envelope"}})(d.value);return function(i){return Cl(o(i))}}(),u=function(){var o=I_({reflectSymbol:function(){return"cancel"}})(d.value);return function(i){return Cl(o(i))}}(),a=function(o){return function(i){return function(f){return function(m){return je({numeric:function(v){return br(f(e(v)))},envelope:function(v){return br(f(n(v)))},cancel:function(v){return br(f(u(v)))},sudden:function(v){return br(f(r(v)))},unit:function(v){var D=wt(pt)(1)([v.u]);return mr(function(A,b){var _=Fe(w.value)();return A(iq(kn({parent:w.value,scope:o,raiseId:function(k){return oq(Cu(new $(k))(_))}})(i)(D))(mr(function(k,V){return function(){var Dt=Ze(_)();if(Dt instanceof w)return void 0;if(Dt instanceof $)return V(f(t({i:Dt.value0})));throw new Error("Failed pattern match at Ocarina.Control (line 1827, column 42 - line 1829, column 80): "+[Dt.constructor.name])}(),An(void 0)})),b)})}})(m)}}}};return a}),Kr=QT(1804),kq=wt(pt),dq=function(t){var r=$k(t);return function(e){return function(n){var u=r(e),a=function(o){return function(i){return mr(function(f,m){var v=i.ids();o.raiseId(v)();var D=f(xe([br(i.makeLoopBuf({id:v,parent:o.parent,scope:ye(o.scope),buffer:u.buffer,playbackRate:u.playbackRate,loopStart:u.loopStart,loopEnd:u.loopEnd,duration:u.duration})),sn(an(function(A){return je({buffer:function(b){return br(i.setBuffer({id:v,buffer:b}))},playbackRate:Kr(o.scope)(i)(function(b){return i.setPlaybackRate(function(_){return{id:v,playbackRate:_}}(b))}),loopStart:function(b){return br(i.setLoopStart({id:v,loopStart:b}))},loopEnd:function(b){return br(i.setLoopEnd({id:v,loopEnd:b}))},onOff:function(b){return br(i.setOnOff({id:v,onOff:b}))}})(A)})(n))]),m);return function(){return m(i.deleteFromCache({id:v})),D()}})}};return new _r(a)}}},tr=function(t){return dq(t)};var yq=function(t){var r=xT(t);return function(e){return function(n){var u=r(e),a=function(o){return function(i){return mr(function(f,m){var v=i.ids();o.raiseId(v)();var D=f(xe([br(i.makePeriodicOsc({id:v,parent:o.parent,scope:ye(o.scope),frequency:u.frequency,spec:u.spec})),sn(an(function(A){return je({frequency:Kr(o.scope)(i)(function(b){return i.setFrequency(function(_){return{id:v,frequency:_}}(b))}),onOff:function(b){return br(i.setOnOff({id:v,onOff:b}))},spec:function(b){return br(i.setPeriodicOsc({id:v,spec:b}))}})(A)})(n))]),m);return function(){return m(i.deleteFromCache({id:v})),D()}})}};return new _r(a)}}},Pl=function(t){return yq(t)};var gq=function(t){var r=xk(t);return function(e){return function(n){var u=r(e),a=function(o){return function(i){return mr(function(f,m){var v=i.ids();o.raiseId(v)();var D=f(xe([br(i.makePlayBuf({id:v,parent:o.parent,scope:ye(o.scope),buffer:u.buffer,playbackRate:u.playbackRate,bufferOffset:u.bufferOffset,duration:u.duration})),sn(an(function(A){return je({buffer:function(b){return br(i.setBuffer({id:v,buffer:b}))},playbackRate:Kr(o.scope)(i)(function(b){return i.setPlaybackRate(function(_){return{id:v,playbackRate:_}}(b))}),bufferOffset:function(b){return br(i.setBufferOffset({id:v,bufferOffset:b}))},onOff:function(b){return br(i.setOnOff({id:v,onOff:b}))},duration:function(b){return br(i.setDuration({id:v,duration:b}))}})(A)})(n))]),m);return function(){return m(i.deleteFromCache({id:v})),D()}})}};return new _r(a)}}},au=function(t){return gq(t)};var hq=function(t){var r=hT(t);return function(e){return function(n){var u=r(e),a=function(o){return function(i){return mr(function(f,m){var v=i.ids();o.raiseId(v)();var D=f(xe([br(i.makeSawtoothOsc({id:v,parent:o.parent,scope:ye(o.scope),frequency:u.frequency})),sn(an(function(A){return je({frequency:Kr(o.scope)(i)(function(b){return i.setFrequency(function(_){return{id:v,frequency:_}}(b))}),onOff:function(b){return br(i.setOnOff({id:v,onOff:b}))}})(A)})(n))]),m);return function(){return m(i.deleteFromCache({id:v})),D()}})}};return new _r(a)}}},KT=function(t){return hq(t)};var Sq=function(t){var r=gT(t);return function(e){return function(n){var u=r(e),a=function(o){return function(i){return mr(function(f,m){var v=i.ids();o.raiseId(v)();var D=f(xe([br(i.makeSinOsc({id:v,parent:o.parent,scope:ye(o.scope),frequency:u.frequency})),sn(an(function(A){return je({frequency:Kr(o.scope)(i)(function(b){return i.setFrequency(function(_){return{id:v,frequency:_}}(b))}),onOff:function(b){return br(i.setOnOff({id:v,onOff:b}))}})(A)})(n))]),m);return function(){return m(i.deleteFromCache({id:v})),D()}})}};return new _r(a)}}},gc=function(t){return Sq(t)},YT=function(t){var r=gc(t);return function(e){return r(e)(su)}},xq=function(t){var r=yT(t);return function(e){return function(n){var u=r(e),a=function(o){return function(i){return mr(function(f,m){var v=i.ids();o.raiseId(v)();var D=f(xe([br(i.makeSquareOsc({id:v,parent:o.parent,scope:ye(o.scope),frequency:u.frequency})),sn(an(function(A){return je({frequency:Kr(o.scope)(i)(function(b){return i.setFrequency(function(_){return{id:v,frequency:_}}(b))}),onOff:function(b){return br(i.setOnOff({id:v,onOff:b}))}})(A)})(n))]),m);return function(){return m(i.deleteFromCache({id:v})),D()}})}};return new _r(a)}}},R_=function(t){return xq(t)},ZT=function(t){var r=R_(t);return function(e){return r(e)(su)}},Cq=function(t){var r=kT(t);return function(e){return function(n){var u=r(e),a=function(o){return function(i){return mr(function(f,m){var v=i.ids();o.raiseId(v)();var D=f(xe([br(i.makeTriangleOsc({id:v,parent:o.parent,scope:ye(o.scope),frequency:u.frequency})),sn(an(function(A){return je({frequency:Kr(o.scope)(i)(function(b){return i.setFrequency(function(_){return{id:v,frequency:_}}(b))}),onOff:function(b){return br(i.setOnOff({id:v,onOff:b}))}})(A)})(n))]),m);return function(){return m(i.deleteFromCache({id:v})),D()}})}};return new _r(a)}}},Sm=function(t){return Cq(t)};var $q=function(t){var r=Mk(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([br(f.makeAllpass({id:D,parent:i.parent,scope:ye(i.scope),frequency:a.frequency,q:a.q})),sn(an(function(b){return je({frequency:Kr(i.scope)(f)(function(_){return f.setFrequency(function(k){return{id:D,frequency:k}}(_))}),q:Kr(i.scope)(f)(function(_){return f.setQ(function(k){return{id:D,q:k}}(_))})})(b)})(n)),kn({parent:new $(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},Rk=function(t){var r=$q(t);return function(e){return function(n){return r(e)(su)(n)}}},Nk=function(t){var r=Ek(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([br(f.makeBandpass({id:D,parent:i.parent,scope:ye(i.scope),frequency:a.frequency,q:a.q})),sn(an(function(b){return je({frequency:Kr(i.scope)(f)(function(_){return f.setFrequency(function(k){return{id:D,frequency:k}}(_))}),q:Kr(i.scope)(f)(function(_){return f.setQ(function(k){return{id:D,q:k}}(_))})})(b)})(n)),kn({parent:new $(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},Xi=function(t){var r=Nk(t);return function(e){return function(n){return r(e)(su)(n)}}},N_=function(t){var r=Fk(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([br(f.makeDelay({id:D,parent:i.parent,scope:ye(i.scope),delayTime:a.delayTime,maxDelayTime:a.maxDelayTime})),sn(an(function(b){return je({delayTime:Kr(i.scope)(f)(function(_){return f.setDelay(function(k){return{id:D,delayTime:k}}(_))})})(b)})(n)),kn({parent:new $(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},hf=function(t){var r=N_(t);return function(e){return function(n){return r(e)(su)(n)}}},Tq=function(t){var r=wT(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([br(f.makeDynamicsCompressor({id:D,parent:i.parent,scope:ye(i.scope),threshold:a.threshold,ratio:a.ratio,knee:a.knee,attack:a.attack,release:a.release})),sn(an(function(b){return je({threshold:Kr(i.scope)(f)(function(_){return f.setThreshold(function(k){return{id:D,threshold:k}}(_))}),ratio:Kr(i.scope)(f)(function(_){return f.setRatio(function(k){return{id:D,ratio:k}}(_))}),knee:Kr(i.scope)(f)(function(_){return f.setKnee(function(k){return{id:D,knee:k}}(_))}),attack:Kr(i.scope)(f)(function(_){return f.setAttack(function(k){return{id:D,attack:k}}(_))}),release:Kr(i.scope)(f)(function(_){return f.setRelease(function(k){return{id:D,release:k}}(_))})})(b)})(n)),kn({parent:new $(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},tF=function(t){var r=Tq(t);return function(e){return r(e)(su)}},Fq=function(){return function(t){return function(r){return fq({doLogic:Jo,ids:function(e){return function(n){return n.ids}(km(e))},disconnectElement:function(e){return function(n){return e.disconnectXFromY({from:n.id,to:n.parent})}},toElt:function(e){return e}})({fromEltO1:P_,fromEltO2:P_,toElt:P_,wrapElt:function(e){return kq(1)([e])},giveNewParent:function(e){return function(n){return function(u){return function(a){return e.connectXToY({from:n.id,to:n.parent})}}}},deleteFromCache:function(e){return function(n){return n.deleteFromCache}(km(e))}})(t)(JT(cq(function(e){return e(void 0)}))(P_(r)))}}},Eq=Fq(),iu=function(t){return function(r){return Eq(Vb(t))(JT(lq(d.value))(r))}};var Uk=function(t){var r=Tk(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([br(f.makeHighpass({id:D,parent:i.parent,scope:ye(i.scope),frequency:a.frequency,q:a.q})),sn(an(function(b){return je({frequency:Kr(i.scope)(f)(function(_){return f.setFrequency(function(k){return{id:D,frequency:k}}(_))}),q:Kr(i.scope)(f)(function(_){return f.setQ(function(k){return{id:D,q:k}}(_))})})(b)})(n)),kn({parent:new $(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},Ll=function(t){var r=Uk(t);return function(e){return function(n){return r(e)(su)(n)}}},Mq=function(t){var r=MT(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([br(f.makeHighshelf({id:D,parent:i.parent,scope:ye(i.scope),frequency:a.frequency,gain:a.gain})),sn(an(function(b){return je({frequency:Kr(i.scope)(f)(function(_){return f.setFrequency(function(k){return{id:D,frequency:k}}(_))}),gain:Kr(i.scope)(f)(function(_){return f.setGain(function(k){return{id:D,gain:k}}(_))})})(b)})(n)),kn({parent:new $(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},rF=function(t){var r=Mq(t);return function(e){return function(n){return r(e)(su)(n)}}},eF=function(t){var r=Ck(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([br(f.makeLowpass({id:D,parent:i.parent,scope:ye(i.scope),frequency:a.frequency,q:a.q})),sn(an(function(b){return je({frequency:Kr(i.scope)(f)(function(_){return f.setFrequency(function(k){return{id:D,frequency:k}}(_))}),q:Kr(i.scope)(f)(function(_){return f.setQ(function(k){return{id:D,q:k}}(_))})})(b)})(n)),kn({parent:new $(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},Rl=function(t){var r=eF(t);return function(e){return function(n){return r(e)(su)(n)}}},Oq=function(t){var r=FT(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([br(f.makeLowshelf({id:D,parent:i.parent,scope:ye(i.scope),frequency:a.frequency,gain:a.gain})),sn(an(function(b){return je({frequency:Kr(i.scope)(f)(function(_){return f.setFrequency(function(k){return{id:D,frequency:k}}(_))}),gain:Kr(i.scope)(f)(function(_){return f.setGain(function(k){return{id:D,gain:k}}(_))})})(b)})(n)),kn({parent:new $(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},nF=function(t){var r=Oq(t);return function(e){return function(n){return r(e)(su)(n)}}},wq=function(t){var r=$T(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([br(f.makeNotch({id:D,parent:i.parent,scope:ye(i.scope),frequency:a.frequency,q:a.q})),sn(an(function(b){return je({frequency:Kr(i.scope)(f)(function(_){return f.setFrequency(function(k){return{id:D,frequency:k}}(_))}),q:Kr(i.scope)(f)(function(_){return f.setQ(function(k){return{id:D,q:k}}(_))})})(b)})(n)),kn({parent:new $(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},uF=function(t){var r=wq(t);return function(e){return function(n){return r(e)(su)(n)}}},Iq=function(t){var r=dT(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([br(f.makeStereoPanner({id:D,parent:i.parent,scope:ye(i.scope),pan:a.pan})),sn(an(function(b){return je({pan:Kr(i.scope)(f)(function(_){return f.setPan(function(k){return{id:D,pan:k}}(_))})})(b)})(n)),kn({parent:new $(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},aF=function(t){var r=Iq(t);return function(e){return r(e)(su)}},Pq=function(t){var r=CT(t);return function(e){return function(n){return function(u){var a=r(e),o=function(i){return function(f){return mr(function(m,v){var D=f.ids();i.raiseId(D)();var A=m(xe([br(f.makePeaking({id:D,parent:i.parent,scope:ye(i.scope),frequency:a.frequency,q:a.q,gain:a.gain})),sn(an(function(b){return je({frequency:Kr(i.scope)(f)(function(_){return f.setFrequency(function(k){return{id:D,frequency:k}}(_))}),q:Kr(i.scope)(f)(function(_){return f.setQ(function(k){return{id:D,q:k}}(_))}),gain:Kr(i.scope)(f)(function(_){return f.setGain(function(k){return{id:D,gain:k}}(_))})})(b)})(n)),kn({parent:new $(D),scope:i.scope,raiseId:function(b){return An(void 0)}})(f)(ze(u))]),v);return function(){return v(f.deleteFromCache({id:D})),A()}})}};return new _r(o)}}}},iF=function(t){var r=Pq(t);return function(e){return function(n){return r(e)(su)(n)}}},oF=function(t){var r=Sk(t);return function(e){return function(n){var u=r(e),a=function(o){return function(i){return mr(function(f,m){var v=i.ids();o.raiseId(v)();var D=f(xe([br(i.makeWaveShaper({id:v,parent:o.parent,scope:ye(o.scope),curve:u.curve,oversample:u.oversample})),kn({parent:new $(v),scope:o.scope,raiseId:function(A){return An(void 0)}})(i)(ze(n))]),m);return function(){return m(i.deleteFromCache({id:v})),D()}})}};return new _r(a)}}},Lq=function(t){var r=PT(t);return function(e){return function(n){var u=r(e),a=function(o){return function(i){return mr(function(f,m){var v=i.ids();o.raiseId(v)();var D=f(xe([br(i.makeConstant({id:v,parent:o.parent,scope:ye(o.scope),offset:u.offset})),sn(an(function(A){return je({offset:Kr(o.scope)(i)(function(b){return i.setOffset(function(_){return{id:v,offset:_}}(b))}),onOff:function(b){return br(i.setOnOff({id:v,onOff:b}))}})(A)})(n))]),m);return function(){return m(i.deleteFromCache({id:v})),D()}})}};return new _r(a)}}},xm=function(t){return Lq(t)};function Bk(){window.scrollTo(0,0)}var on=E("button");var fF=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),U_=function(){function t(){}return t.value=new t,t}(),hc=function(){function t(){}return t.value=new t,t}(),B_=function(){function t(){}return t.value=new t,t}(),Sc=function(){function t(){}return t.value=new t,t}(),W_=function(){function t(){}return t.value=new t,t}(),q_=function(){function t(){}return t.value=new t,t}(),cF=function(){function t(){}return t.value=new t,t}(),Cm=function(){function t(){}return t.value=new t,t}(),$m=function(){function t(){}return t.value=new t,t}(),H_=function(){function t(){}return t.value=new t,t}(),z_=function(){function t(){}return t.value=new t,t}(),lF=function(){function t(){}return t.value=new t,t}(),Nl=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Wk=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var Nq="numeric",Uq="sudden",Bq="unit",Wq="cancel",qq="step",Hq="linear",zq="exponential",Vq="envelope",pF=function(t,r,e,n){if(e.type===Uq)t.value=e.value.n;else if(e.type===Bq)r.id&&Jq(r.id,n),n.units[e.value.i].main.connect(t),r.id=e.value.i;else if(e.type===Nq)t[e.value.t.type===qq?"setValueAtTime":e.value.t.type===Hq?"linearRampToValueAtTime":e.value.t.type===zq?"exponentialRampToValueAtTime":"linearRampToValueAtTime"](e.value.n,e.value.o);else if(e.type===Wq)e.value.hold?t.cancelAndHoldAtTime(e.value.o):t.cancelScheduledValues(e.value.o);else if(e.type===Vq){let u=e.value.o;t.cancelScheduledValues(Math.max(0,u)),t.setValueCurveAtTime(e.value.p,u,e.value.d)}else throw new Error("No idea what to do with "+JSON.stringify(e))},Gq=function(t,r,e,n,u){return n[e]||(n[e]={}),pF(r.parameters.get(e),n[e],u,t)},La=function(t,r,e,n,u){return n[e]||(n[e]={}),pF(r[e],n[e],u,t)},le=function(t,r,e,n){let u=t("@fan@")(a=>a)(e);n.scopes[u]||(n.scopes[u]=[]),n.scopes[u].push(r),n.units[r].scope=u},pe=function(t,r){r.toConnect[t]&&(r.toConnect[t].forEach(function(e){e.w?r.units[e.w]?e.f():(r.toConnect[e.w]||(r.toConnect[e.w]=[]),r.toConnect[e.w].push({f:e.f})):e.f()}),delete r.toConnect[t])},_e=function(t,r,e,n){t()(u=>_F(r,u,n))(e)},_F=function(t,r,e){var n=function(){e.units[t].audioOutgoing.push(r),e.units[t].pendingOn||(e.units[t].main.connect(e.units[r].main),e.units[r].se&&e.units[t].main.connect(e.units[r].se))};if(!e.units[t]){e.toConnect[t]||(e.toConnect[t]=[]);var u={f:n};r!==t&&!e.units[r]&&(u.w=r),e.toConnect[t].push(u);return}if(!e.units[r]){e.toConnect[r]||(e.toConnect[r]=[]);var u={f:n};r!==t&&!e.units[t]&&(u.w=t),e.toConnect[r].push(u);return}n()};function qk(t){return function(r){return function(){delete r.units[t.id]}}}function Hk(t){return function(r){return function(){_F(t.from,t.to,r)}}}var Jq=function(t,r){if(r.units[t].scope==="@fan@")return;let e=r.units[t].scope;r.scopes[e].forEach(n=>{delete r.units[n]}),delete r.scopes[e]};function zk(t){return function(r){return function(){var e=t.from,n=t.to;if(!r.units[e]||(r.units[e].audioOutgoing=r.units[e].audioOutgoing.filter(function(a){return a!==n}),r.units[e].main.disconnect(r.units[n].main),r.units[n].se&&r.units[e].main.disconnect(r.units[n].se),r.units[e].scope==="@fan@"))return;let u=r.units[e].scope;r.scopes[u].forEach(a=>{delete r.units[a]}),delete r.scopes[u]}}}var Vk=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"allpass",Q:r.q,frequency:r.frequency})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},Gk=t=>r=>e=>()=>{var n=r.id,u=r.cb,a=new AnalyserNode(e.context,r),o=u(a)();e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],analyserOrig:u,analyser:o,main:e.context.createGain(),se:a},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},Jk=t=>r=>e=>()=>{var n=r.id,u=r.options;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new AudioWorkletNode(e.context,u.name,{numberOfInputs:u.numberOfInputs,numberOfOutputs:u.numberOfOutputs,outputChannelCount:u.outputChannelCount,parameterData:u.parameterData,processorOptions:u.processorOptions})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},jk=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"bandpass",Q:r.q,frequency:r.frequency})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},Xk=t=>r=>e=>()=>{var n=r.id,u=function(o,i){return new ConstantSourceNode(o,i)},a={offset:r.offset};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:u,onOff:!1,pendingOn:!0,main:u(e.context,a)},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},Qk=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new ConvolverNode(e.context,{buffer:r.buffer})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},Kk=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DelayNode(e.context,{delayTime:r.delayTime,maxDelayTime:r.maxDelayTime})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},Yk=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DynamicsCompressorNode(e.context,{knee:r.knee,ratio:r.ratio,threshold:r.threshold,attack:r.attack,release:r.release})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},Zk=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new GainNode(e.context,{gain:r.gain})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},td=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highpass",Q:r.q,frequency:r.frequency})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},rd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highshelf",frequency:r.frequency,gain:r.gain})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},ed=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new IIRFilterNode(e.context,{feedforward:r.feedforward,feedback:r.feedback})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},nd=t=>r=>e=>()=>{var n=r.id,u=function(o,i){return new AudioBufferSourceNode(o,i)},a={loop:!0,buffer:r.buffer,loopStart:r.loopStart,loopEnd:r.loopEnd,playbackRate:r.playbackRate};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:u,onOff:!1,pendingOn:!0,main:u(e.context,a)},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},ud=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowpass",Q:r.q,frequency:r.frequency})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},ad=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowshelf",frequency:r.frequency,gain:r.gain})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},id=t=>r=>e=>()=>{var n=r.id,u=r.element,a=function(){var o=e.context.createMediaElementSource(u);return o};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],createClosure:a,resumeClosure:{},main:a()},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},od=t=>r=>e=>()=>{var n=r.id;e.units[r.id]={main:e.context.createMediaStreamSource(r.microphone),controllers:{},audioOutgoing:[],controlOutgoing:[]},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},fd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"notch",frequency:r.frequency,Q:r.q})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},cd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"peaking",frequency:r.frequency,Q:r.q,gain:r.gain})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},ld=t=>r=>e=>()=>{var n=r.id,u=function(o,i){var f={frequency:i.frequency,periodicWave:i.spec.type==="wave"?i.spec.value:jd(e.context)(i.spec.value.real)(i.spec.value.img)()},m=new OscillatorNode(o,f);return m},a={frequency:r.frequency,type:"custom",spec:r.spec};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:u,onOff:!1,pendingOn:!0,main:u(e.context,a)},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},pd=t=>r=>e=>()=>{var n=r.id,u=function(o,i){var f={loop:i.loop,buffer:i.buffer,playbackRate:i.playbackRate};return new AudioBufferSourceNode(o,f)},a={loop:!1,buffer:r.buffer,playbackRate:r.playbackRate,bufferOffset:r.bufferOffset,duration:t(void 0)(o=>o)(r.duration)};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:u,onOff:!1,pendingOn:!0,main:u(e.context,a)},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},_d=t=>r=>e=>()=>{var n=r.id,u=r.cb,a=e.context.createMediaStreamDestination(),o=new MediaRecorder(a.stream);u(o)(),o.start(),e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],recorderOrig:u,recorder:o,main:e.context.createGain(),se:a},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},vd=t=>r=>e=>()=>{var n=r.id,u=function(o,i){return new OscillatorNode(o,i)},a={frequency:r.frequency,type:"sawtooth"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:u,onOff:!1,pendingOn:!0,main:u(e.context,a)},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},sd=t=>r=>e=>()=>{var n=r.id,u=function(o,i){return new OscillatorNode(o,i)},a={frequency:r.frequency,type:"sine"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:u,onOff:!1,pendingOn:!0,main:u(e.context,a)},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},md=t=>r=>()=>{r.units[t.id]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:r.context.createGain(),se:r.context.destination}},Dd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new StereoPannerNode(e.context,{pan:r.pan})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},bd=t=>r=>e=>()=>{var n=r.id,u=function(o,i){return new OscillatorNode(o,i)},a={frequency:r.frequency,type:"square"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:u,onOff:!1,pendingOn:!0,main:u(e.context,a)},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},Ad=t=>r=>e=>()=>{var n=r.id,u=function(o,i){return new OscillatorNode(o,i)},a={frequency:r.frequency,type:"triangle"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:u,onOff:!1,pendingOn:!0,main:u(e.context,a)},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)},kd=t=>r=>e=>()=>{var n=r.id,u=r.curve,a=r.oversample;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new WaveShaperNode(e.context,{curve:u,oversample:a.type})},le(t,n,r.scope,e),pe(n,e),_e(t,n,r.parent,e)};function dd(t){return function(r){return function(){var e=t.id,n=t.cb;r.units[e].analyserOrig!==n&&(r.units[e].analyser&&r.units[e].analyser(),r.units[e].analyser=n(r.units[e].se)(),r.units[e].analyserOrig=n)}}}function yd(t){return function(r){return function(){var e=t.cb,n=t.id;if(r.units[n].recorderOrig!==e){r.units[n].recorder&&r.units[n].recorder.stop();var u=e;r.units[n].recorderOrig=e;var a=new MediaRecorder(r.units[n].se);u(a)(),a.start()}}}}function gd(t){return function(r){return function(){var e=t.id,n=t.curve;r.units[e].main.curve=n}}}function hd(t){return function(r){return function(){var e=t.id,n=t.paramName,u=t.paramValue;Gq(r,r.units[e].main,n,r.units[e].controllers,u)}}}var Ra=function(t,r,e){r.resume&&t.value.n!==void 0&&(r.resume[e]=t.value.n)};function Sd(t){return function(r){return function(){var e=t.id,n=t.gain;La(r,r.units[e].main,"gain",r.units[e].controllers,n),Ra(n,r.units[e],"gain")}}}function xd(t){return function(r){return function(){var e=t.id,n=t.q;La(r,r.units[e].main,"Q",r.units[e].controllers,n),Ra(n,r.units[e],"Q")}}}function Cd(t){return function(r){return function(){var e=t.id,n=t.buffer;r.units[e].resume&&(r.units[e].resume.buffer=n)}}}function $d(t){return function(r){return function(){var e=t.id,n=t.buffer;r.units[e].main.buffer=n}}}function Td(t){return function(r){return function(){var e=t.id,n=t.spec;r.units[e].resume&&(r.units[e].resume.spec=n)}}}function Fd(t){return function(r){return function(){var e=t.id,n=t.pan;La(r,r.units[e].main,"pan",r.units[e].controllers,n),Ra(n,r.units[e],"pan")}}}function Ed(t){return function(r){return function(){var e=t.id,n=t.threshold;La(r,r.units[e].main,"threshold",r.units[e].controllers,n),Ra(n,r.units[e],"threshold")}}}function Md(t){return function(r){return function(){var e=t.id,n=t.loopStart;r.units[e].main.loopStart=n,r.units[e].resume.loopStart=n}}}function Od(t){return function(r){return function(){var e=t.id,n=t.loopEnd;r.units[e].main.loopEnd=n,r.units[e].resume.loopEnd=n}}}function wd(t){return function(r){return function(){var e=t.id,n=t.bufferOffset;r.units[e].resume.bufferOffset=n}}}function Id(t){return function(r){return function(e){return function(){var n=r.id,u=r.duration;e.units[n].duration=t(void 0)(a=>a)(u)}}}}function Pd(t){return function(r){return function(){var e=t.id,n=t.release;La(r,r.units[e].main,"release",r.units[e].controllers,n),Ra(n,r.units[e],"release")}}}function Ld(t){return function(r){return function(){var e=t.id,n=t.offset;La(r,r.units[e].main,"offset",r.units[e].controllers,n),Ra(n,r.units[e],"offset")}}}function Rd(t){return function(r){return function(){var e=t.id,n=t.ratio;La(r,r.units[e].main,"ratio",r.units[e].controllers,n),Ra(n,r.units[e],"ratio")}}}function Nd(t){return function(r){return function(){var e=t.id,n=t.attack;La(r,r.units[e].main,"attack",r.units[e].controllers,n),Ra(n,r.units[e],"attack")}}}function Ud(t){return function(r){return function(){var e=t.id,n=t.knee;La(r,r.units[e].main,"knee",r.units[e].controllers,n),Ra(n,r.units[e],"knee")}}}function Bd(t){return function(r){return function(){var e=t.id,n=t.delayTime;La(r,r.units[e].main,"delayTime",r.units[e].controllers,n),Ra(n,r.units[e],"delayTime")}}}function Wd(t){return function(r){return function(){var e=t.id,n=t.playbackRate;La(r,r.units[e].main,"playbackRate",r.units[e].controllers,n),Ra(n,r.units[e],"playbackRate")}}}function qd(t){return function(r){return function(){var e=t.id,n=t.frequency;La(r,r.units[e].main,"frequency",r.units[e].controllers,n),Ra(n,r.units[e],"frequency")}}}function Hd(t){return function(r){return function(){var e=t.id,n=t.onOff;n.x.type==="on"?jq(e)(n)(r)():n.x.type==="off"&&Xq(e)(n)(r)()}}}var jq=function(t){return function(r){return function(e){return function(){if(!e.units[t].onOff){e.units[t].pendingOn=!1,e.units[t].onOff=!0,e.units[t].main=e.units[t].createClosure(e.context,e.units[t].resume);for(var n=0;n<e.units[t].audioOutgoing.length;n++){var u=e.units[t].audioOutgoing[n];e.units[t].main.connect(e.units[u].main),e.units[u].se&&e.units[t].main.connect(e.units[u].se)}e.units[t].resume&&e.units[t].resume.bufferOffset?typeof e.units[t].resume.duration=="number"?e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.bufferOffset,e.units[t].resume.duration):e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.bufferOffset):e.units[t].resume&&e.units[t].resume.loopStart?e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.loopStart):e.units[t].main.start(e.deprecatedWriteHead+r.o)}}}}},Xq=function(t){return function(r){return function(e){return function(){if(!!e.units[t].onOff){e.units[t].onOff=!1;var n=e.units[t].main;n.addEventListener("ended",()=>{n.disconnect()}),n.stop(e.deprecatedWriteHead+r.o)}}}}};function zd(t){for(var r=new Float32Array(t.length),e=0;e<t.length;e++)r[e]=t[e];return r}function Tm(t){return function(){t.stop()}}function Vd(t){return function(r){return function(e){return function(){var n=[];e.ondataavailable=function(u){n.push(u.data)},e.onstop=function(){var u=new Blob(n,{type:t});r(u)(),n=null}}}}}function Gd(t){return function(r){return function(){return navigator.mediaDevices.getUserMedia({audio:t,video:r})}}}function V_(t){return function(){var r=new Uint8Array(t.frequencyBinCount);return t.getByteFrequencyData(r),r}}function Jd(t){return function(){var r=t.createConstantSource();return r.offset.value=0,r.connect(t.destination),r.start(),function(){r.stop(),r.disconnect(t.destination)}}}var jd=function(t){return function(r){return function(e){return function(){for(var n=new Float32Array(r.length),u=new Float32Array(e.length),a=0;a<r.length;a++)n[a]=r[a];for(var a=0;a<e.length;a++)u[a]=e[a];return t.createPeriodicWave(n,u,{disableNormalization:!0})}}}};function Sf(t){return function(){return{context:t,deprecatedWriteHead:0,units:{},scopes:{},unsu:{},toConnect:{}}}}function Xd(t){return function(){t.close()}}function Qd(t){return function(){return fetch(t).then(function(r){return r.arrayBuffer()},function(r){return console.error("Error fetching buffer",r),Promise.reject(r)})}}function Kd(t){return function(r){return function(){return t.decodeAudioData(r)}}}function Yd(){return new(window.AudioContext||window.webkitAudioContext)}function Zd(t){return function(){return t.state}}function G_(t){return function(){return t.currentTime}}function vF(t){return function(r){return function(e){return function(){t.then(e,r)}}}}var Yq=xv(L),Zq=Nt(dC),t4=Q(Z),r4=gt(vA(bb)(_a)),e4=pk(_a),n4=g(v_(Va)),u4=_k(_a),a4=ct(qn),i4=de(Ge),o4=function(t){return function(r){return ll(function(e){return Yq(Zq)(vF(r)(function(n){return e(St.create(t(n)))()})(function(n){return e(xt.create(n))()}))})}};var f4=function(t){return jn(function(r){return ni("Promise failed, couldn't extract JS Error or String")})(t4)(ik(r4(e4("Error")(t))(n4(ni)(u4(t)))))},sF=o4(f4),Fm=function(t){return a4(i4(t))(sF)};function ty(t){return function(){return URL.createObjectURL(t)}}var s4=Ft(gu);var m4=cn(Qn);var D4=sc(Xs),b4=Ir(Kn),A4=Er(pa),k4=ct(qn);var d4=In(Bt),y4=g(To),mF=F(no);var DF=function(t){return function(r){return function(e){return Gt(Vd(t))(e)(function(){var n=m4(r);return function(u){return n(ty(u))}}())}}};var xc=function(t){return{ids:function(){var e=Ze(t)(),n=s4(bf(D4)({newSeed:Df(e),size:5}));return b4(ja(A4(1))(t))(),n},deleteFromCache:qk,disconnectXFromY:zk,connectXToY:Hk,makeAllpass:Vk(yt),makeAnalyser:Gk(yt),makeAudioWorkletNode:Jk(yt),makeBandpass:jk(yt),makeConstant:Xk(yt),makeConvolver:Qk(yt),makeDelay:Kk(yt),makeDynamicsCompressor:Yk(yt),makeGain:Zk(yt),makeHighpass:td(yt),makeHighshelf:rd(yt),makeIIRFilter:ed(yt),makeLoopBuf:nd(yt),makeLowpass:ud(yt),makeLowshelf:ad(yt),makeMediaElement:id(yt),makeMicrophone:od(yt),makeNotch:fd(yt),makePeaking:cd(yt),makePeriodicOsc:ld(yt),makePlayBuf:pd(yt),makeRecorder:_d(yt),makeSawtoothOsc:vd(yt),makeSinOsc:sd(yt),makeSpeaker:md,makeSquareOsc:bd(yt),makeStereoPanner:Dd(yt),makeTriangleOsc:Ad(yt),makeWaveShaper:kd(yt),setAnalyserNodeCb:dd,setMediaRecorderCb:yd,setWaveShaperCurve:gd,setAudioWorkletParameter:hd,setBuffer:Cd,setConvolverBuffer:$d,setDuration:Id(yt),setPeriodicOsc:Td,setOnOff:Hd,setBufferOffset:wd,setLoopStart:Md,setLoopEnd:Od,setRatio:Rd,setOffset:Ld,setAttack:Nd,setGain:Sd,setQ:xd,setPan:Fd,setThreshold:Ed,setRelease:Pd,setKnee:Ud,setDelay:Bd,setPlaybackRate:Wd,setFrequency:qd}},ut=function(t){return function(r){return k4(Fm(Qd(r)))(function(){var e=Kd(t);return function(n){return Fm(e(n))}}())}},J_=function(t){var r=de(t);return function(e){return r(Zd(e))}},g4=J_(ne);var Mn=function(t){return de(t)(Yd)},Na=function(t){var r=de(t);return function(e){return r(Jd(e))}},mn=function(t){var r=de(t);return function(e){return r(function(){var u=g4(e)();return d4(u!=="closed")(Xd(e))()})}},h4=J,S4=J,Em=function(t){return function(r){return y4(function(e){return{microphone:function(){return t?mF(h4(e)):w.value}(),camera:function(){return r?mF(S4(e)):w.value}()}})(Fm(Gd(t)(r)))}};var x4=ai(Ku),C4=gt(yC),bF=Qu(Ku),Mm=ct(qn),AF=de(Ge),ey=st(ke),j_=rr(or),X_=g(ft),ry=F(Bt),$4=Mn(Ge),T4=Na(Ge),F4=mn(ne),E4=at(pn),kF=gt(Wt),dF=F(j),M4=st(As),Ki=function(){function t(){}return t.value=new t,t}(),Yi=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),ua=function(){function t(){}return t.value=new t,t}(),Xe=Bk,Lo=function(t){return x4(C4(bF(Mm(kC(t))(AF)))(bF(AA(ni("We navigated away from the page"))(t))))},Q_=function(t){var r=gt(t);return function(e){var n=F(e);return function(u){return function(a){return r(n(ua.value))(a)}}}},yF=Q_(Wt)(j),ou=function(t){var r=gt(t),e=g(t.Functor0());return function(n){var u=F(n);return function(a){return function(o){return r(u(ey(ee.value)(Vr(M(o)))))(e(function(i){return ey(ee.value)(Vr(M(j_(i)(o))))})(e(function(i){return i.value0})(a)))}}}},Om=function(t){return function(r){return function(e){return function(n){return function(u){return function(a){return X_(function(o){return ey(ee.value)(Vr(M(function(){if(o.value0 instanceof Ki)return ry(void 0);if(o.value0 instanceof Yi)return j_(j_(o.value0.value0)(t(ry(void 0))))(r(ua.value));if(o.value0 instanceof ua)return function(){o.value1(),r(Ki.value)();var f=Ca(Mm($4)(function(m){return Mm(T4(m))(function(v){return Mm(e(m))(function(D){return AF(function(){var b=n(m)(D)(),_=j_(j_(b)(v))(F4(m));return r(new Yi(_))(),_})})})}))();return t(function(){return r(ua.value)(),ii(Lo(f))()})(),void 0};throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 57, column 21 - line 75, column 26): "+[o.value0.constructor.name])}())))})(E4(X_(N.create)(a))(kF(dF(ry(void 0)))(X_(function(o){return o.value0})(u))))}}}}}},fu=function(t){return function(r){return function(e){return function(){return t(e)(),r(new fF(e))()}}}},wm=function(t){return function(r){return function(e){return function(n){return function(u){return Be(function(a){return function(o){var i=yF(r)(o);return cl([kF(dF(M4(Mt.value)("cursor: pointer;")))(Om(e)(a)(n)(u)(r)(i))])([Ve(X_(function(f){if(f instanceof ua)return t;if(f instanceof Ki)return"\u23F3";if(f instanceof Yi)return"\u{1F6D1}";throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 126, column 17 - line 129, column 35): "+[f.constructor.name])})(i))])}})}}}}},_t=function(t){return function(r){return function(e){return function(n){return Be(function(u){return function(a){var o=yF(t)(a);return on([Om(r)(u)(e)(n)(t)(o)])([Ve(X_(function(i){if(i instanceof ua)return"Turn on";if(i instanceof Ki)return"Loading...";if(i instanceof Yi)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 100, column 17 - line 103, column 42): "+[i.constructor.name])})(o))])}})}}}};var gF=Aa(ba),O4=g(ft),w4=Mn(ne),I4=g(L),P4=rr(or),L4=mn(ne),Ul=function(t){return function(r){return function(){var n=Sf(t)(),u=gF(Fe(0))(),a=we(yc([new ri(O4(function(o){return Yp.create(NS(o))})(r))])(xc(u)))(function(o){return o(n)})();return a}}};var Y=function(t){return function(r){return function(){var n=Sf(t)(),u=gF(Fe(0))(),a=we(yc(r)(xc(u)))(function(o){return o(n)})();return a}}},Im=function(t){return function(){var e=w4();return I4(function(n){return P4(n)(L4(e))})(Y(e)(t))()}};var R4=Yt({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(W()(R)({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}})),N4=tr(cr),U4=X(),B4=wt(pt),hF=Rk(HT),SF=Rk(Pk(Zt(Cr()(kt(kt(xr)(AT)()()()({reflectSymbol:function(){return"q"}}))(hk)()()()({reflectSymbol:function(){return"frequency"}})))(Sr()()))),W4=function(){return d.value}(),xF=function(t){return function(r){return function(e){return R4(d.value)(W4)({allpass:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([iu(N4(u)(U4))(function(a){return B4(.2)([a,hF(700)([SF({frequency:990,q:20})([a]),hF(1110)([a,SF({frequency:2010,q:30})([a])])])])})])}})})}}};function Ro(t){return function(e,n,u){if(n===null)return new t(e);var a=e.byteLength,o=t.BYTES_PER_ELEMENT,i=Math.min(a,n>>>0);if(u===null)return new t(e,i);var f=Math.min((a-i)/o,u);return new t(e,i,f)}}var H4=Ro(Uint8ClampedArray),z4=Ro(Uint32Array),V4=Ro(Uint16Array),CF=Ro(Uint8Array),G4=Ro(Int32Array),J4=Ro(Int16Array),j4=Ro(Int8Array),X4=Ro(Float32Array),Q4=Ro(Float64Array);function $F(t){for(var r=t.length,e=new Array(r),n=0;n<r;n++)e[n]=t[n];return e}var Pm={create:CF,BinaryValue0:function(){}};var Lm=function(t){return function(r){return function(){return $F(r)}}};var Bl=Yu,Wl=Yu,ql=Yu,Iu=Yu,Pu=Yu,Lu=Yu,Ru=Yu,Nu=Yu;function Rm(t){return t|0}var eH=function(t,r,e){var n=0,u;return function(a){if(n===2)return u;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+a+")",r,a);return n=1,u=e(),n=2,u}},nH=Ir(L),uH=In(Bt),xf=Cn(function(t){return function(){var e=Fo(),n=Or(!0)(),u=eH("fx","FRP.Event.Animate",function(){return nH(Gt(Ys)(e)(function(){var i=Ur(n)();return uH(i)(function(){return t(void 0)(),u(19)()})()}))}),a=u(15);return a(),te(!1)(n)}});var OF=uk(Zu),iy=Ie(Se(i0)()(Io)()(ek)),oy=Ie(Se(fi)()(ta)()(Io)),fy=Ie(Se(Mo)()(ra)()(ta)),cy=Ie(Se(Oo)()(ea)()(ra)),ly=Ie(Se(wo)()(Po)()(ea)),aH=Cr(),iH=kt(kt(xr)(ym)()()()({reflectSymbol:function(){return"fftSize"}})),oH={reflectSymbol:function(){return"cb"}},fH=Sr()(),cH=X(),lH=Ja(Ev),pH=mt({reflectType:function(){return`<section>
  <h2 id="analyser">Analyser</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode">analyser node</a> provides methods to recuperate the analysed data of an input. This is how, for example, Google Meet shows the little animation around a microphone icon. Ocarina provides the possibility to use the analyser as the terminus of an audio graph <i>or</i> as part of a longer DSP chain, as in the following example. The example uses an FFT size of 256, which is indicated in Ocarina as <code>TTT8</code> (two to the eighth power).</p>

  <pre><code>analyser_ { cb, fftSize: TTT8 } [ loopBuf atar bangOn ]</code></pre>

  ~analyser~
  </section>
`}})()()(W()(R)({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}})),_H=Qv(ya),vH=Q(Z),sH=Q_(Wt)(j),MF=gt(Wt),gr=F(j),mH=st(ol),DH=Aa(ba),ny=g(ft),bH=$u(Bt)(Me),AH=Lm(Pm),uy=Er(pa),kH=en(f0)(Bt),ay=g(L),dH=Ie(Se(n0)()(tk)()(o0)),yH=Ie(Se(u0)()(rk)()(tk)),gH=Ie(Se(a0)()(ek)()(rk)),hH=J_(ne),SH=In(Bt),xH=mn(ne),CH=uk(uu),$r=st(kr),$H="background-color: rgb(150,30,10);",TH="background-color: rgb(130,60,10);",FH="background-color: rgb(80,90,10);",EH="background-color: rgb(10,130,10);",MH="background-color: rgb(10,100,0);",OH=OF(function(t){return iy($H)(oy(TH)(fy(FH)(cy(EH)(ly(MH)(Oa)))))}),wH=function(t){var r=hm(gm(Zt(aH(iH(t)()()()(oH)))(fH)));return function(e){var n=tr(e);return function(u){return function(a){return r({cb:a,fftSize:fm.value})([n(u)(cH)])}}}},IH=wH(dm)(cr),PH=function(){return d.value}(),Tr="background-color: rgb(255,255,255,0.0);",LH=function(t){var r=g(t);return function(e){var n=nk(e)();return function(u){return function(a){var o=nk(a)();return function(i){return function(f){var m=st(f);return function(v){return function(D){return function(A){return function(b){return function(_){return r(function(k){var V=n(o(k)(A))(b);return V?m(Mt.value)(n(o(OH)(A))(b)):m(Mt.value)(Tr)})(_)}}}}}}}}}}},K_=LH(ft),Cf=K_(vn)(Gi),RH=Cf(vn)(Di)(kr)(Gi)(Di),NH=Cf(Ma)(mi)(kr)(Gi)(mi),UH=Cf(Ea)(si)(kr)(Gi)(si),BH=Cf(Fa)(vi)(kr)(Gi)(vi),WH=Cf(Ta)(_i)(kr)(Gi)(_i),qH=Cf(uu)(pi)(kr)(Gi)(pi),HH=Cf(Wi)(li)(kr)(Gi)(li),zH=Cf(Bi)(ci)(kr)(Gi)(ci),$f=K_(Ma)(Vi),VH=$f(vn)(Di)(kr)(Vi)(Di),GH=$f(Ma)(mi)(kr)(Vi)(mi),JH=$f(Ea)(si)(kr)(Vi)(si),jH=$f(Fa)(vi)(kr)(Vi)(vi),XH=$f(Ta)(_i)(kr)(Vi)(_i),QH=$f(uu)(pi)(kr)(Vi)(pi),KH=$f(Wi)(li)(kr)(Vi)(li),YH=$f(Bi)(ci)(kr)(Vi)(ci),Tf=K_(Ea)(zi),ZH=Tf(vn)(Di)(kr)(zi)(Di),tz=Tf(Ma)(mi)(kr)(zi)(mi),rz=Tf(Ea)(si)(kr)(zi)(si),ez=Tf(Fa)(vi)(kr)(zi)(vi),nz=Tf(Ta)(_i)(kr)(zi)(_i),uz=Tf(uu)(pi)(kr)(zi)(pi),az=Tf(Wi)(li)(kr)(zi)(li),iz=Tf(Bi)(ci)(kr)(zi)(ci),Ff=K_(Fa)(Hi),oz=Ff(vn)(Di)(kr)(Hi)(Di),fz=Ff(Ma)(mi)(kr)(Hi)(mi),cz=Ff(Ea)(si)(kr)(Hi)(si),lz=Ff(Fa)(vi)(kr)(Hi)(vi),pz=Ff(Ta)(_i)(kr)(Hi)(_i),_z=Ff(uu)(pi)(kr)(Hi)(pi),vz=Ff(Wi)(li)(kr)(Hi)(li),sz=Ff(Bi)(ci)(kr)(Hi)(ci),Ef=K_(Ta)(qi),mz=Ef(vn)(Di)(kr)(qi)(Di),Dz=Ef(Ma)(mi)(kr)(qi)(mi),bz=Ef(Ea)(si)(kr)(qi)(si),Az=Ef(Fa)(vi)(kr)(qi)(vi),kz=Ef(Ta)(_i)(kr)(qi)(_i),dz=Ef(uu)(pi)(kr)(qi)(pi),yz=Ef(Wi)(li)(kr)(qi)(li),gz=Ef(Bi)(ci)(kr)(qi)(ci),hz=function(){return 15/40}(),Sz=function(){return 10/40}(),xz=function(){return 7/40}(),Cz=function(){return 3/40}(),$z=function(){return 1/40}(),wF=function(t){return function(r){return function(e){return pH(PH)({analyser:Be(function(n){return function(u){var a=_H(vH)(u),o=sH(e)(function(f){return f.right}(a)),i=function(f){return f.left}(a);return Rr([on([MF(gr(mH(Mt.value)("cursor: pointer;")))(Om(t)(function(f){return n(xt.create(f))})(function(f){return ut(f)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(f){return function(m){return function(){var D=Or(w.value)(),A=Sf(f)(),b=DH(Fe(0))(),_=yc([IH(m)(function(V){return function(){return te(new $(V))(D)(),te(w.value)(D)}})])(xc(b)),k=we(MF(ny(xt.create)(_))(ny(St.create)(xf)))(function(V){if(V instanceof xt)return V.value0(A);if(V instanceof St)return function(){var Dt=Ur(D)();return bH(Dt)(function(vr){return function(){var H=V_(vr)(),Ht=AH(H)(),vt=Or(0)(),qr=Or(0)(),Wr=Or(0)(),fn=Or(0)(),On=Or(0)(),ia=Or(0)(),be=Or(0)(),ue=Or(0)(),Wu=Or(0)(),to=Or(0)(),Ec=function(dn){if(dn<32)return vt;if(dn<64)return qr;if(dn<96)return Wr;if(dn<128)return fn;if(dn<168)return On;if(dn<160)return ia;if(dn<224)return be;if(Jr)return ue;throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 142, column 45 - line 150, column 63): "+[dn.constructor.name])};bp(Ht)(function(dn){var Vf=Rm(dn);return function(){var ro=Ur(to)();return oo(uy(Vf))(Wu)(),oo(uy(Vf))(Ec(ro))(),oo(uy(1))(to)()}})();var lu=kH(function(dn){return function(){var Sv=ay(Br)(Ur(dn))(),ro=ay(lH(Sv))(ay(Br)(Ur(Wu)))();return iy(ro>hz)(oy(ro>Sz)(fy(ro>xz)(cy(ro>Cz)(ly(ro>$z)(Oa)))))}})(dH(vt)(yH(qr)(gH(Wr)(iy(fn)(oy(On)(fy(ia)(cy(be)(ly(ue)(Oa)))))))))();return n(new St(lu))()}})()};throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 123, column 57 - line 161, column 57): "+[V.constructor.name])})();return function(){return k(),function(){var Dt=hH(f)();return SH(Dt!=="closed")(xH(f))()}(),n(new St(OF(M(CH(M(!1))))))()}}}})(e)(o))])([Ve(ny(function(f){if(f instanceof ua)return"Turn on";if(f instanceof Ki)return"Loading...";if(f instanceof Yi)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 174, column 31 - line 177, column 56): "+[f.constructor.name])})(o))]),sr([gr($r(Mt.value)("display: grid; grid-template-columns: repeat(8, 1fr); grid-auto-rows: 20px;"))])([sr([gr($r(Mt.value)(Tr)),RH(Nu)(Nu)(i)])([]),sr([gr($r(Mt.value)(Tr)),NH(Ru)(Nu)(i)])([]),sr([gr($r(Mt.value)(Tr)),UH(Lu)(Nu)(i)])([]),sr([gr($r(Mt.value)(Tr)),BH(Pu)(Nu)(i)])([]),sr([gr($r(Mt.value)(Tr)),WH(Iu)(Nu)(i)])([]),sr([gr($r(Mt.value)(Tr)),qH(ql)(Nu)(i)])([]),sr([gr($r(Mt.value)(Tr)),HH(Wl)(Nu)(i)])([]),sr([gr($r(Mt.value)(Tr)),zH(Bl)(Nu)(i)])([]),sr([gr($r(Mt.value)(Tr)),VH(Nu)(Ru)(i)])([]),sr([gr($r(Mt.value)(Tr)),GH(Ru)(Ru)(i)])([]),sr([gr($r(Mt.value)(Tr)),JH(Lu)(Ru)(i)])([]),sr([gr($r(Mt.value)(Tr)),jH(Pu)(Ru)(i)])([]),sr([gr($r(Mt.value)(Tr)),XH(Iu)(Ru)(i)])([]),sr([gr($r(Mt.value)(Tr)),QH(ql)(Ru)(i)])([]),sr([gr($r(Mt.value)(Tr)),KH(Wl)(Ru)(i)])([]),sr([gr($r(Mt.value)(Tr)),YH(Bl)(Ru)(i)])([]),sr([gr($r(Mt.value)(Tr)),ZH(Nu)(Lu)(i)])([]),sr([gr($r(Mt.value)(Tr)),tz(Ru)(Lu)(i)])([]),sr([gr($r(Mt.value)(Tr)),rz(Lu)(Lu)(i)])([]),sr([gr($r(Mt.value)(Tr)),ez(Pu)(Lu)(i)])([]),sr([gr($r(Mt.value)(Tr)),nz(Iu)(Lu)(i)])([]),sr([gr($r(Mt.value)(Tr)),uz(ql)(Lu)(i)])([]),sr([gr($r(Mt.value)(Tr)),az(Wl)(Lu)(i)])([]),sr([gr($r(Mt.value)(Tr)),iz(Bl)(Lu)(i)])([]),sr([gr($r(Mt.value)(Tr)),oz(Nu)(Pu)(i)])([]),sr([gr($r(Mt.value)(Tr)),fz(Ru)(Pu)(i)])([]),sr([gr($r(Mt.value)(Tr)),cz(Lu)(Pu)(i)])([]),sr([gr($r(Mt.value)(Tr)),lz(Pu)(Pu)(i)])([]),sr([gr($r(Mt.value)(Tr)),pz(Iu)(Pu)(i)])([]),sr([gr($r(Mt.value)(Tr)),_z(ql)(Pu)(i)])([]),sr([gr($r(Mt.value)(Tr)),vz(Wl)(Pu)(i)])([]),sr([gr($r(Mt.value)(Tr)),sz(Bl)(Pu)(i)])([]),sr([gr($r(Mt.value)(Tr)),mz(Nu)(Iu)(i)])([]),sr([gr($r(Mt.value)(Tr)),Dz(Ru)(Iu)(i)])([]),sr([gr($r(Mt.value)(Tr)),bz(Lu)(Iu)(i)])([]),sr([gr($r(Mt.value)(Tr)),Az(Pu)(Iu)(i)])([]),sr([gr($r(Mt.value)(Tr)),kz(Iu)(Iu)(i)])([]),sr([gr($r(Mt.value)(Tr)),dz(ql)(Iu)(i)])([]),sr([gr($r(Mt.value)(Tr)),yz(Wl)(Iu)(i)])([]),sr([gr($r(Mt.value)(Tr)),gz(Bl)(Iu)(i)])([])])])}})})}}};var Fz=Yt({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(W()(R)({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}})),Ez=tr(cr),Mz=X(),Oz=wt(pt),Y_=Xi(ki(Zt(Cr()(kt(kt(xr)(ji)()()()({reflectSymbol:function(){return"q"}}))(bi)()()()({reflectSymbol:function(){return"frequency"}})))(Sr()()))),wz=function(){return d.value}(),IF=function(t){return function(r){return function(e){return Fz(d.value)(wz)({bandpass:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([iu(Ez(u)(Mz))(function(a){return Oz(.8)([Y_({frequency:400,q:1})([a]),Y_({frequency:880,q:5})([a]),Y_({frequency:1200,q:10})([a]),Y_({frequency:2e3,q:20})([a]),Y_({frequency:3e3,q:30})([a])])})])}})})}}};var Pz=mt({reflectType:function(){return`<section>
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
`}})()()(W()(R)({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}})),Lz=tF(WT(Zt(Cr()(xr))(Sr()()))),Rz=tr(cr),Nz=X(),Uz=function(){return d.value}(),PF=function(t){return function(r){return function(e){return Pz(Uz)({compression:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([Lz({})([Rz(u)(Nz)])])}})})}}};var Mf=po(),Of=Eo();var Wz=Of({reflectSymbol:function(){return"playbackRate"}}),qz=Of({reflectSymbol:function(){return"onOff"}}),Hz=Of({reflectSymbol:function(){return"offset"}}),zz=Of({reflectSymbol:function(){return"loopStart"}}),Vz=Of({reflectSymbol:function(){return"loopEnd"}}),Gz=Of({reflectSymbol:function(){return"gain"}}),Jz=Of({reflectSymbol:function(){return"frequency"}});var jz=Of({reflectSymbol:function(){return"delayTime"}});var Uu=function(){return function(t){var r=Wz(d.value),e=dc(t);return function(n){return Mf(r(e(n)))}}},Cc=function(){return function(t){var r=qz(d.value),e=U0(t);return function(n){return Mf(r(e(n)))}}},LF=function(){return function(t){var r=Hz(d.value),e=dc(t);return function(n){return Mf(r(e(n)))}}},RF=function(){var t=zz(d.value);return function(r){return Mf(t(r))}},NF=function(){var t=Vz(d.value);return function(r){return Mf(t(r))}},Du=function(){return function(t){var r=Gz(d.value),e=dc(t);return function(n){return Mf(r(e(n)))}}},wf=function(){return function(t){var r=Jz(d.value),e=dc(t);return function(n){return Mf(r(e(n)))}}};var py=function(){return function(t){var r=jz(d.value),e=dc(t);return function(n){return Mf(r(e(n)))}}};var _y=W(),Xz=mt({reflectType:function(){return`<section>
  <h2 id="constant">Constant value</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode">Constant values</a>, or DC offset, is a way to output an unchanging stream of values. This is only really useful when testing the performance of speakers or microphones and/or when working with a custom audio node that supports constant streaming values. Note that the constant source node in the web audio API can <i>also</i> be used to control audio parameters. Ocarina uses this feature of constant nodes under the hood to optimize certain computations.</p>

  <p>The following example abuses a constant audio node by turning it into a gnarly inpulse generator. We'll learn about the tie fighter symbol <code>~tf~</code> and the <code>pure</code> in the next section on Events. Kids, don't try this at home!</p>

  <pre><code>~txt~</code></pre>

  ~constant~
  </section>
`}})()()(_y(_y(_y(R)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"tf"}})({reflectSymbol:function(){return"tf"}}))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}})),Qz=F(ru),Kz=wt(pt),Yz=xm(sm),Zz=gt(Wt),tV=X(),rV=F(j),eV=LF()(En),nV=Tu(Da),uV=hu(Ga),aV=function(){return d.value}(),UF=function(t){return function(r){return function(e){return Xz(aV)({tf:S("<|>"),txt:S(`run2_
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
  ]`),constant:_t(e)(t)(function(n){return Qz(void 0)})(function(n){return function(u){return Y(n)([Kz(.5)([Yz(0)(Zz(tV)(rV(eV({d:5,o:.1,p:nV(function(a){return M(function(){var o=uV(a)(3)===0;return o?1:0}())})(Ke(0)(1920))}))))])])}})})}}};var oV=mt({reflectType:function(){return`<section>
  <h2 id="convolution">Convolution</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConvolverNode">Convolution</a>, aka reverb, is a way to graft the shape of one sound (usually an <a href="https://en.wikipedia.org/wiki/Impulse_response">impulse response</a>) onto another. Convolution can sound great, but it is a <i>very expensive operation</i> that will cause noticeable artifacts on low-end devices. When shipping audio code to production, you're usually better off using an Audio Worklet Node with reverb optimized for your specific case. That said, for PoCs or hobbyist projects, convolution is great!</p>

  <pre><code>\\{loop, verb} -> run2_
  [ convolver verb [ loopBuf loop bangOn ] ]</code></pre>

  ~convolution~
  </section>
`}})()()(W()(R)({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}})),fV=at(DC),cV=g(To),lV=jT(eT),pV=tr(cr),_V=X(),vV=function(){return d.value}(),BF=function(t){return function(r){return function(e){return oV(vV)({convolution:_t(e)(t)(function(n){return fV(cV(function(u){return function(a){return{loop:u,verb:a}}})(ut(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")))(ut(n)("https://cdn.jsdelivr.net/gh/andibrae/Reverb.js/Library/StMarysAbbeyReconstructionPhase3.m4a"))})(function(n){return function(u){return Y(n)([lV(u.verb)([pV(u.loop)(_V)])])}})})}}};var mV=Yt({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(W()(R)({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}})),DV=au(Pa),bV=X(),AV=wt(pt),Um=hf(Ai),kV=function(){return d.value}(),WF=function(t){return function(r){return function(e){return mV(d.value)(kV)({delay:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(u){return Y(n)([iu(DV(u)(bV))(function(a){return AV(.2)([Um(.03)([a]),Um(.1)([a]),Um(.3)([a]),Um(.7)([a])])})])}})})}}};var yV=mt({reflectType:function(){return`<section>
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
`}})()()(W()(R)({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}})),gV=wt(pt),hV=tr(cr),SV=X(),xV=function(){return d.value}(),qF=function(t){return function(r){return function(e){return yV(xV)({gain:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(u){return Y(n)([gV(.1)([hV(u)(SV)])])}})})}}};var $V=mt({reflectType:function(){return`<section>
  <h2 id="highpass">Highpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highpass filter</a> lets higher frequencies pass and amortizes lower ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ highpass_ 2000.0
      [ loopBuf buf bangOn ]
  ]
</code></pre>

  ~highpass~
  </section>
`}})()()(W()(R)({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}})),TV=Ll(Il),FV=tr(cr),EV=X(),MV=function(){return d.value}(),HF=function(t){return function(r){return function(e){return $V(MV)({highpass:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([TV(2e3)([FV(u)(EV)])])}})})}}};var wV=mt({reflectType:function(){return`<section>
  <h2 id="highshelf">Highshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highshelf filter</a> boosts or attenuates high frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
  [ highshelf_ { frequency: 2000.0, gain: 0. }
      [ loopBuf buf bangOn ]
  ]</code></pre>

  ~highshelf~
  </section>
`}})()()(W()(R)({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}})),IV=rF(BT(Zt(Cr()(kt(kt(xr)(sT)()()()({reflectSymbol:function(){return"gain"}}))(mT)()()()({reflectSymbol:function(){return"frequency"}})))(Sr()()))),PV=tr(cr),LV=X(),RV=function(){return d.value}(),zF=function(t){return function(r){return function(e){return wV(RV)({highshelf:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([IV({frequency:2e3,gain:.4})([PV(u)(LV)])])}})})}}};var UV=mt({reflectType:function(){return`<section>
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
`}})()()(W()(R)({reflectType:function(){return"iirFilterEx"}})({reflectSymbol:function(){return"iirFilterEx"}})),BV=XT()()(rT(lc)(lc)),Hl=ux()(),WV=tr(cr),qV=X(),HV=function(){return d.value}(),VF=function(t){return function(r){return function(e){return UV(HV)({iirFilterEx:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([BV(new N(Hl(20298e-8)(Hl(.0004059599)(Hl(20298e-8)(Jb))),Hl(1.0126964558)(Hl(-1.9991880801)(Hl(.9873035442)(Jb)))))([WV(u)(qV)])])}})})}}};var VV=Yt({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(W()(R)({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}})),JF=Cr(),jF=kt(kt(xr)(bm)()()()({reflectSymbol:function(){return"playbackRate"}})),XF={reflectSymbol:function(){return"buffer"}},QF=Sr()(),GF=tr(wl(Zt(JF(kt(kt(jF(Dm)()()()({reflectSymbol:function(){return"loopStart"}}))(mm)()()()({reflectSymbol:function(){return"loopEnd"}}))(El)()()()(XF)))(QF))),vy=X(),GV=tr(wl(Zt(JF(jF(El)()()()(XF)))(QF))),JV=function(){return d.value}(),KF=function(t){return function(r){return function(e){return VV(d.value)(JV)({loopBuf:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/100/100981_1234256-lq.mp3")})(function(n){return function(u){return Y(n)([GF({buffer:u,playbackRate:.5,loopStart:.1,loopEnd:.6})(vy),GF({buffer:u,playbackRate:1,loopStart:.5,loopEnd:1.2})(vy),GV({buffer:u,playbackRate:1.7})(vy)])}})})}}};var XV=mt({reflectType:function(){return`<section>
  <h2 id="lowpass">Lowpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowpass filter</a> lets lower frequencies pass and amortizes higher ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ lowpass_ 215.0 [ loopBuf buf bangOn ] ]
</code></pre>

  ~lowpass~
  </section>
`}})()()(W()(R)({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}})),QV=Rl(Am),KV=tr(cr),YV=X(),ZV=function(){return d.value}(),YF=function(t){return function(r){return function(e){return XV(ZV)({lowpass:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([QV(215)([KV(u)(YV)])])}})})}}};var r5=mt({reflectType:function(){return`<section>
  <h2 id="lowshelf">Lowshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowshelf filter</a> boosts or attenuates lower frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
   [ lowshelf_ { frequency: 91.0, gain: 10.0 }
       [ loopBuf buf bangOn ]
   ]
</code></pre>

  ~lowshelf~
  </section>
`}})()()(W()(R)({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}})),e5=nF(UT(Zt(Cr()(kt(kt(xr)(pT)()()()({reflectSymbol:function(){return"gain"}}))(_T)()()()({reflectSymbol:function(){return"frequency"}})))(Sr()()))),n5=tr(cr),u5=X(),a5=function(){return d.value}(),ZF=function(t){return function(r){return function(e){return r5(a5)({lowshelf:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([e5({frequency:91,gain:.4})([n5(u)(u5)])])}})})}}};var o5=Yt({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(W()(R)({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}})),sy=wt(pt),f5=L_(M_),c5=hf(Ai),l5=YT(gf),p5=function(){return d.value}(),tE=function(t){return function(r){return function(e){return o5(d.value)(p5)({microphone:_t(e)(t)(function(n){return Em(!0)(!1)})(function(n){return function(u){return Y(n)([function(){if(u.microphone instanceof $)return wu(function(a){return sy(1)([f5(u.microphone.value0),c5(.1)([sy(.2)([a])])])});if(u.microphone instanceof w)return sy(.02)([l5(440)]);throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Microphone (line 44, column 15 - line 49, column 56): "+[u.microphone.constructor.name])}()])}})})}}};var v5=mt({reflectType:function(){return`<section>
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
`}})()()(W()(R)({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}})),Z_=uF(NT(Zt(Cr()(kt(kt(xr)(cT)()()()({reflectSymbol:function(){return"q"}}))(lT)()()()({reflectSymbol:function(){return"frequency"}})))(Sr()()))),tv=F(Ha),s5=tr(cr),m5=X(),D5=function(){return d.value}(),rE=function(t){return function(r){return function(e){return v5(D5)({notch:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([Z_({frequency:400,q:1})(tv(Z_({frequency:880,q:5})(tv(Z_({frequency:1200,q:10})(tv(Z_({frequency:2e3,q:20})(tv(Z_({frequency:3e3,q:30})(tv(s5(u)(m5)))))))))))])}})})}}};var A5=mt({reflectType:function(){return`<section>
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
`}})()()(W()(R)({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}})),rv=iF(RT(Zt(Cr()(kt(kt(kt(xr)(iT)()()()({reflectSymbol:function(){return"q"}}))(oT)()()()({reflectSymbol:function(){return"gain"}}))(fT)()()()({reflectSymbol:function(){return"frequency"}})))(Sr()()))),ev=F(Ha),k5=tr(cr),d5=X(),y5=function(){return d.value}(),eE=function(t){return function(r){return function(e){return A5(y5)({peaking:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([rv({frequency:400,q:1,gain:-20})(ev(rv({frequency:880,q:5,gain:20})(ev(rv({frequency:1200,q:10,gain:-20})(ev(rv({frequency:2e3,q:20,gain:20})(ev(rv({frequency:3e3,q:30,gain:-20})(ev(k5(u)(d5)))))))))))])}})})}}};var h5=mt({reflectType:function(){return`<section>
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
`}})()()(W()(R)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})),S5=F(ru),x5=wt(pt),C5=Pl(Ol(Zt(Cr()(kt(kt(xr)(Ml($l(fi)))()()()({reflectSymbol:function(){return"spec"}}))(Fl)()()()({reflectSymbol:function(){return"frequency"}})))(Sr()()))),nE=Ie(Se(fi)()(ta)()(Io)),uE=Ie(Se(Mo)()(ra)()(ta)),aE=Ie(Se(Oo)()(ea)()(ra)),iE=Ie(Se(wo)()(Po)()(ea)),$5=X(),T5=function(){return d.value}(),oE=function(t){return function(r){return function(e){return h5(T5)({periodic:_t(e)(t)(function(n){return S5(void 0)})(function(n){return function(u){return Y(n)([x5(.2)([C5({frequency:140,spec:new N(nE(.1)(uE(.2)(aE(.3)(iE(.4)(Oa)))),nE(.4)(uE(.3)(aE(.2)(iE(.1)(Oa)))))})($5)])])}})})}}};var E5=mt({reflectType:function(){return`<section>
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
`}})()()(W()(R)({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}})),M5=au(w_(Zt(Cr()(kt(kt(kt(xr)(uT)()()()({reflectSymbol:function(){return"duration"}}))(nT)()()()({reflectSymbol:function(){return"bufferOffset"}}))(O_)()()()({reflectSymbol:function(){return"buffer"}})))(Sr()()))),O5=X(),w5=function(){return d.value}(),fE=function(t){return function(r){return function(e){return E5(w5)({playBuf:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/470/470035_9564355-lq.mp3")})(function(n){return function(u){return Y(n)([M5({buffer:u,duration:3,bufferOffset:4.2})(O5)])}})})}}};var my=function(){function t(){}return t.value=new t,t}();var cE={attr:function(t){return function(r){return c({key:"controls",value:s(r)})}}};var Dy=function(){function t(){}return t.value=new t,t}();var lE={attr:function(t){return function(r){return c({key:"src",value:s(r)})}}};var by=E("audio");var $c=function(){function t(){this.head=null,this.last=null,this.size=0}function r(v,D){this.queue=v,this.value=D,this.next=null,this.prev=null}function e(v){this.draining=!1,this.error=null,this.value=v,this.takes=new t,this.reads=new t,this.puts=new t}var n={};function u(v){try{v()}catch(D){setTimeout(function(){throw D},0)}}function a(v,D){var A=new r(v,D);switch(v.size){case 0:v.head=A;break;case 1:A.prev=v.head,v.head.next=A,v.last=A;break;default:A.prev=v.last,v.last.next=A,v.last=A}return v.size++,A}function o(v){var D;switch(v.size){case 0:return null;case 1:D=v.head,v.head=null;break;case 2:D=v.last,v.head.next=null,v.last=null;break;default:D=v.last,v.last=D.prev,v.last.next=null}return D.prev=null,D.queue=null,v.size--,D.value}function i(v){var D;switch(v.size){case 0:return null;case 1:D=v.head,v.head=null;break;case 2:D=v.head,v.last.prev=null,v.head=v.last,v.last=null;break;default:D=v.head,v.head=D.next,v.head.prev=null}return D.next=null,D.queue=null,v.size--,D.value}function f(v){if(v.queue!==null){if(v.queue.last===v){o(v.queue);return}if(v.queue.head===v){i(v.queue);return}v.prev&&(v.prev.next=v.next),v.next&&(v.next.prev=v.prev),v.queue.size--,v.queue=null,v.value=null,v.next=null,v.prev=null}}function m(v,D){if(!D.draining){var A=D.puts,b=D.takes,_=D.reads,k,V,et,Dt,vr;for(D.draining=!0;;){if(k=null,V=null,et=null,Dt=D.value,vr=_.size,D.error!==null){for(Dt=v.left(D.error);k=i(A);)u(k.cb(Dt));for(;V=i(_);)u(V(Dt));for(;et=i(b);)u(et(Dt));break}if(Dt===n&&(k=i(A))&&(D.value=Dt=k.value),Dt!==n){for(et=i(b);vr--&&(V=i(_));)u(V(v.right(Dt)));et!==null&&(D.value=n,u(et(v.right(Dt))))}if(k!==null&&u(k.cb(v.right(void 0))),D.value===n&&A.size===0||D.value!==n&&b.size===0)break}D.draining=!1}}return e.EMPTY=n,e.putLast=a,e.takeLast=o,e.takeHead=i,e.deleteCell=f,e.drainVar=m,e}();function Ay(){return new $c($c.EMPTY)}function pE(t,r,e){return function(){return e.value===$c.EMPTY&&e.error===null?(e.value=r,$c.drainVar(t,e),!0):!1}}function _E(t,r){return function(){var e=r.value;return e===$c.EMPTY?t.nothing:(r.value=$c.EMPTY,$c.drainVar(t,r),t.just(e))}}var U5=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),B5=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),W5=function(){function t(){}return t.value=new t,t}();var vE=function(){return{left:St.create,right:xt.create,nothing:w.value,just:$.create,killed:U5.create,filled:B5.create,empty:W5.value}}();var sE=function(t){return function(r){return pE(vE,t,r)}};var mE=function(t){return _E(vE,t)};var H5=mt({reflectType:function(){return`<section>
  <h2 id="recorder">Recorder</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioDestinationNode">recorder</a> allows you to record audio. It takes a callback that you can use to stash the recorded audio somewhere, like in a file for example, as the example below does. You can use it as a simple note-taking app \u{1F399}\uFE0F.</p>

  <pre><code>\\cb m -> recorder cb (microphone m)</code></pre>

  ~recorder~
  </section>
`}})()()(W()(R)({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}})),DE=Qv(ya),bE=Q(Z),z5=Q_(Wt)(j),zl=gt(Wt),nv=F(j),V5=st(ol),If=g(ft),G5=st(ke),uv=F(Bt),ky=rr(or),J5=$u(Bt)(Me),AE=ds(eC),j5=ct(Qn),X5=ct(qn),Q5=g(To),K5=de(Ge),Y5=Mn(ne),Z5=Aa(ba),tG=Ir(L),rG=Dn(Bt)(Me),eG=J_(ne),nG=In(Bt),uG=mn(ne),kE=at(pn),aG=st(cE),dE=st(Hx),iG=st(lE),oG=function(t){var r=Lk(t);return function(e){var n=L_(e);return function(u){return function(a){return r(a)(n(u))}}}},fG=oG(kk)(M_),cG=function(){return d.value}(),yE=function(t){return function(r){return function(e){return H5(cG)({recorder:Be(function(n){return function(u){var a=DE(bE)(u),o=DE(bE)(function(v){return v.left}(a)),i=function(v){return v.right}(o),f=z5(e)(function(v){return v.right}(a)),m=function(v){return v.left}(o);return Rr([on([zl(nv(V5(Mt.value)("cursor: pointer;")))(If(function(v){return G5(ee.value)(Vr(M(function(){if(v.e instanceof Ki)return uv(void 0);if(v.e instanceof Yi)return ky(ky(ky(v.e.value0)(t(uv(void 0))))(J5(v.rec)(function(D){return AE(Tm(D))})))(n(xt.create(ua.value)));if(v.e instanceof ua)return function(){v.cncl();var A=Ay();n(new xt(Ki.value))();var b=Ca(X5(Q5(function(_){return _.microphone})(Em(!0)(!1)))(function(_){return K5(function(){var V=yt(uv(uv(void 0)))(function(et){return function(){var vr=Y5(),Gr=Sf(vr)(),H=Z5(Fe(0))(),Ht=yc([fG(et)(function(qr){return function(){return n(new St(new xt(qr)))(),tG(sE(qr)(A))(),DF("audio/ogg; codecs=opus")(function(fn){return n(St.create(St.create(fn)))})(qr)()}})])(xc(H)),vt=we(Ht)(function(qr){return qr(Gr)})();return function(){vt(),j5(mE(A))(rG(function(fn){return AE(Tm(fn))}))();var Wr=eG(vr)();return nG(Wr!=="closed")(uG(vr))()}}})(_)();return n(new xt(new Yi(V)))(),V})}))();return t(function(){return n(xt.create(ua.value))(),ii(Lo(b))()})(),void 0};throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Recorder (line 67, column 45 - line 111, column 50): "+[v.e.constructor.name])}())))})(kE(If(Mc)(kE(If(function(v){return function(D){return function(A){return{e:v,cncl:D,rec:A}}}})(f))(zl(nv(uv(void 0)))(If(function(v){return v.value0})(e)))))(zl(nv(w.value))(If($.create)(i)))))])([Ve(If(function(v){if(v instanceof ua)return"Turn on";if(v instanceof Ki)return"Loading...";if(v instanceof Yi)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Recorder (line 123, column 29 - line 126, column 54): "+[v.constructor.name])})(f))]),Rr([by([zl(nv(aG(my.value)("true")))(zl(nv(dE(Mt.value)("display:none;")))(zl(If(function(v){return iG(Dy.value)(v)})(m))(If(M(dE(Mt.value)("display:block;")))(m))))])([])])])}})})}}};var pG=mt({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sawtoothOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(W()(R)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})),_G=F(ru),vG=wt(pt),sG=KT(tT),mG=X(),DG=function(){return d.value}(),gE=function(t){return function(r){return function(e){return pG(DG)({periodic:_t(e)(t)(function(n){return _G(void 0)})(function(n){return function(u){return Y(n)([vG(.2)([sG(448)(mG)])])}})})}}};var AG=mt({reflectType:function(){return`<section>
  <h2 id="sine">Sine wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sine wave oscillator</a> plays back a sine wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sinOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(W()(R)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})),kG=F(ru),dG=wt(pt),yG=gc(gf),gG=X(),hG=function(){return d.value}(),hE=function(t){return function(r){return function(e){return AG(hG)({periodic:_t(e)(t)(function(n){return kG(void 0)})(function(n){return function(u){return Y(n)([dG(.2)([yG(448)(gG)])])}})})}}};var xG=mt({reflectType:function(){return`<section>
  <h2 id="sawtooth">Square wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ squareOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(W()(R)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})),CG=F(ru),$G=wt(pt),TG=R_(Tl),FG=X(),EG=function(){return d.value}(),SE=function(t){return function(r){return function(e){return xG(EG)({periodic:_t(e)(t)(function(n){return CG(void 0)})(function(n){return function(u){return Y(n)([$G(.2)([TG(448)(FG)])])}})})}}};var OG=mt({reflectType:function(){return`<section>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/StereoPannerNode">stereo panner</a> pans audio in the stereo plane. <code>-1.0</code> represents hard left, and <code>1.0</code> represents hard right, as in the example below.</p>

  <pre><code>\\buf -> run2_
  [ pan_ 1.0 [ loopBuf buf bangOn ] ]</code></pre>

  ~pan~
  </section>
`}})()()(W()(R)({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}})),wG=aF(Z0),IG=tr(cr),PG=X(),LG=function(){return d.value}(),xE=function(t){return function(r){return function(e){return OG(LG)({pan:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(u){return Y(n)([wG(1)([IG(u)(PG)])])}})})}}};var NG=function(){return d.value}(),CE=mt({reflectType:function(){return`<ul>
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
`}})()()(R)(NG)({});var BG=mt({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ triangleOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(W()(R)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})),WG=F(ru),qG=wt(pt),HG=Sm(vm),zG=X(),VG=function(){return d.value}(),$E=function(t){return function(r){return function(e){return BG(VG)({periodic:_t(e)(t)(function(n){return WG(void 0)})(function(n){return function(u){return Y(n)([qG(.2)([HG(448)(zG)])])}})})}}};var TE=W(),JG=mt({reflectType:function(){return`<section>
  <h2 id="waveshaper">Waveshaper</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/WaveshaperNode">waveshaper node</a>, aka distortion, uses a <a href="https://en.wikipedia.org/wiki/Waveshaper">waveshaping function</a> to add warmth to a sound.</p>

  <pre><code>~code~</code></pre>

  ~waveShaper~
  </section>
`}})()()(TE(TE(R)({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}}))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}})),jG=g(ir),XG=PD(za)(jf),QG=oF(LT),KG=tr(cr),YG=X(),ZG=function(){return d.value}(),FE=function(t){return function(r){return function(e){return JG(ZG)({code:S(`do
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
    [ waveShaper (makeFloatArray (makeDistortionCurve 400.0)) [ loopBuf buf bangOn ] ]`),waveShaper:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(u){var a=function(o){var i=sf/180;return jG(function(f){var m=Br(f)*2/Br(44100)-1;return(3+o)*m*20*i/(sf+o*XG(m))})(Ke(0)(44099))};return Y(n)([QG(zd(a(400)))([KG(u)(YG)])])}})})}}};var r8=rr(or),ve=W(),e8=mt({reflectType:function(){return`<div>
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
</div>`}})()()(ve(ve(ve(ve(ve(ve(ve(ve(ve(ve(ve(ve(ve(ve(ve(ve(Je()(ve(ve(ve(ve(ve(ve(ve(ve(ve(ve(ve(ve(R)({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}}))({reflectType:function(){return"triangleOsc"}})({reflectSymbol:function(){return"triangleOsc"}}))({reflectType:function(){return"toc"}})({reflectSymbol:function(){return"toc"}}))({reflectType:function(){return"squareOsc"}})({reflectSymbol:function(){return"squareOsc"}}))({reflectType:function(){return"sinOsc"}})({reflectSymbol:function(){return"sinOsc"}}))({reflectType:function(){return"sawtoothOsc"}})({reflectSymbol:function(){return"sawtoothOsc"}}))({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}}))({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}}))({reflectType:function(){return"periodicOsc"}})({reflectSymbol:function(){return"periodicOsc"}}))({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}}))({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}}))({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}}))({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}}))({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}}))({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}}))({reflectType:function(){return"iirFilter"}})({reflectSymbol:function(){return"iirFilter"}}))({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}}))({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}}))({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}}))({reflectType:function(){return"drumroll"}})({reflectSymbol:function(){return"drumroll"}}))({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}}))({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}}))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}}))({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}}))({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}}))({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}}))({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}})),n8=wt(pt),u8=tr(cr),a8=X(),i8=ou(Wt)(j),o8=function(){return d.value}(),EE=function(t){return function(r){return function(e){return function(n){var u=r8(r(Sc.value))(Xe),a=fu(t)(e);return e8(o8)({drumroll:wm("\u{1F941}")(n)(a)(function(o){return ut(o)("https://freesound.org/data/previews/50/50711_179538-lq.mp3")})(function(o){return function(i){return Y(o)([n8(1)([u8(i)(a8)])])}}),toc:CE,allpass:xF(a)(r)(n),analyser:wF(a)(r)(n),bandpass:IF(a)(r)(n),constant:UF(a)(r)(n),compression:PF(a)(r)(n),convolution:BF(a)(r)(n),delay:WF(a)(r)(n),gain:qF(a)(r)(n),highpass:HF(a)(r)(n),highshelf:zF(a)(r)(n),iirFilter:VF(a)(r)(n),loopBuf:KF(a)(r)(n),lowshelf:ZF(a)(r)(n),lowpass:YF(a)(r)(n),notch:rE(a)(r)(n),playBuf:fE(a)(r)(n),peaking:eE(a)(r)(n),microphone:tE(a)(r)(n),pan:xE(a)(r)(n),periodicOsc:oE(a)(r)(n),recorder:yE(a)(r)(n),sawtoothOsc:gE(a)(r)(n),sinOsc:hE(a)(r)(n),squareOsc:SE(a)(r)(n),triangleOsc:$E(a)(r)(n),waveShaper:FE(a)(r)(n),next:i8(n)(u)})}}}};var dy=function(){function t(){}return t.value=new t,t}(),ME={attr:function(t){return function(r){return c({key:"checked",value:s(r)})}}};var di=function(){function t(){}return t.value=new t,t}();var Zi={attr:function(t){return function(r){return c({key:"type",value:s(r)})}}};var yi=E("input");var p8=fa(gi);var _8=function(t){return t},Bm=function(t){var r=Ao(t),e=t.Alternative0(),n=gt(e.Plus1().Alt0()),u=F(e.Applicative0());return function(a){return function(o){return r(n(u(a))(o))}}};var ov=function(t){return function(r){return t(r)}},Vl=function(t){var r=g(t);return{map:function(e){return function(n){return function(u){return n(r(function(a){return function(o){return a(e(o))}})(u))}}}}},No=function(t){var r=g(Vl(t)),e=g(t);return function(n){return function(u){return function(a){return ov(r(n)(u))(e(Jf)(a))}}}};var Wm=function(t){return No(t)(M)};var Bu=_8;var OE=function(t){var r=ka(t),e=t.Alternative0(),n=gt(e.Plus1().Alt0()),u=F(e.Applicative0()),a=g(t.Filterable1().Functor1());return function(o){return function(i){return Bu(function(f){return r(n(u(ov(o)(f)))(a(function(m){return ov(m)(f)})(i)))})}}},wE=function(t){var r=g(t),e=Vl(t);return{apply:function(n){return function(u){return function(a){return u(n(r(p8)(a)))}}},Functor0:function(){return e}}};var IE=ct(Qn),PE=In(Bt),v8=Xo(za),LE=Dn(Bt)(Me),RE=rr(or);var Gl=function(t){return function(r){return Cn(function(e){return we(r)(function(n){return function(){var a=G_(t)();return e({acTime:a,value:n})()}})})}};var NE=function(t){return function(r){return function(e){var n=function(u){return function(a){return function(o){return function(i){return function(f){return function(m){return function(){var D=Ur(o)();return PE(D)(function(){var b=G_(t)(),_=Zv(i$(v8(a-b-.04)(.01)*1e3))(function(){var V=Ur(o)();return PE(V)(function(){return te(a)(f)(),u(a)(),n(u)(a+m)(o)(i)(f)(m)()})()})();return te(new $(_))(i)()})()}}}}}}};return Cn(function(u){return function(){var o=Or(!0)(),i=Or(w.value)(),f=G_(t)(),m=Or(f+r)();n(u)(r)(o)(i)(m)(r)();var v=we(e)(function(D){return function(){IE(Ur(i))(LE(zp))();var b=Ur(m)();return n(u)(b+D)(o)(i)(m)(D)()}})();return RE(RE(v)(te(!1)(o)))(IE(Ur(i))(LE(zp)))}})}}};var cu=function(t){return function(r){return function(e){return function(n){return function(u){var a=e===t||n===r;if(a)return r;var o=(n-r)/(e-t),i=r-o*t;return o*u+i}}}}};var UE=W(),s8=mt({reflectType:function(){return`<section>
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

</section>`}})()()(UE(UE(R)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}})),m8=Pi()(Mi({reflectSymbol:function(){return"cbx"}})()()()(De({reflectSymbol:function(){return"cbx0"}})()()(De({reflectSymbol:function(){return"cbx1"}})()()(De({reflectSymbol:function(){return"cbx2"}})()()(De({reflectSymbol:function(){return"cbx3"}})()()(Yn)()())()())()())()())(Mi({reflectSymbol:function(){return"startStop"}})()()()(De({reflectSymbol:function(){return"start"}})()()(De({reflectSymbol:function(){return"stop"}})()()(Yn)()())()())(Yn)()())()()),BE=gt(Wt),zm=F(j),D8=Bm(me),b8=da(me),WE=_u(qt)(Dr),Lf=g(ft),qE=st(ke),Vm=oe(ft),A8=at(pn),Jl=B(ft),k8=Q(Z),HE=F(Bt),d8=Mn(ne),y8=Na(ne),g8=Qc(me),h8=Er(Jn),S8=No(ft),zE=Ao(me),VE=Wm(ft),x8=mu(pt),C8=Du()(Ia),$8=Pl(Ol(Zt(Cr()(kt(kt(xr)(Ml($l(fi)))()()()({reflectSymbol:function(){return"spec"}}))(Fl)()()()({reflectSymbol:function(){return"frequency"}})))(Sr()()))),GE=Ie(Se(fi)()(ta)()(Io)),JE=Ie(Se(Mo)()(ra)()(ta)),jE=Ie(Se(Oo)()(ea)()(ra)),XE=Ie(Se(wo)()(Po)()(ea)),yy=rn(qt)(Dr),T8=X(),F8=wf()(Ia),fv=rr(or),E8=mn(ne),M8=st(kr),O8=g(ir),w8=st(Zi),I8=st(ME),P8=$D(ir),L8=function(){return d.value}(),QE=function(t){return function(r){return function(e){return function(n){return s8(L8)({txt:S(`module Main where

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
  )`),empl:m8(d.value)(function(u){return function(a){var o=BE(zm(void 0))(a.startStop.start),i=function(A){return D8(!1)(b8(function(b){return function(_){return!b}})(!1)(A))},f=i(a.cbx.cbx3),m=i(a.cbx.cbx2),v=i(a.cbx.cbx1),D=i(a.cbx.cbx0);return Rr([on([WE(Lf(function(){var A=qE(ee.value);return function(b){return A(Vr(M(b)))}}()))([Vm(A8(Jl(o)(k8))(BE(zm(HE(void 0)))(Lf(function(A){return A.value0})(n))))(function(A){return function(){A();var _=d8(),k=y8(_)(),V=function(vr){return function(Gr){return function(H){return g8(function(Ht){return function(vt){var qr=Ht.value1+(vt.value1-Ht.value0)*function(){return vt.value0?vr:1}();return new N(new N(vt.value1,qr),qr)}})(new N(0,0))(S8(N.create)(Gr)(H))}}},et=Ul(_)(ti(Lf(function(){var vr=h8(.04);return function(Gr){return vr(function(H){return H.acTime}(Gr))}}())(Gl(_)(xf)))(function(vr){var Gr=function(Wr){return function(fn){return zE(vr)(Lf(Mc)(zE(fn)(Lf(function(On){return function(ia){return function(be){return{f:On,a:ia,t:be}}}})(Wr))))}},H=Lf(function(Wr){return Wr?4:1})(VE(f)(vr)),Ht=V(4)(m)(vr),vt=Lf(function(Wr){return Wr?4:1})(VE(v)(vr)),qr=V(8)(D)(vr);return[x8(0)(Vm(Gr(qr)(vt))(function(Wr){return C8({n:cu(1)(.01)(4)(.15)(Wr.a)*Ls(sf*Wr.f)+.15,o:Wr.t,t:Ji})}))([$8({frequency:325.6,spec:new N(GE(.3)(JE(-.1)(jE(.7)(XE(-.4)(Oa)))),GE(.6)(JE(.3)(jE(.2)(XE(0)(Oa)))))})(yy([T8,Vm(Gr(Ht)(H))(function(Wr){return F8({n:325.6+cu(1)(3)(4)(15.5)(Wr.a)*Ls(sf*Wr.f),o:Wr.t,t:Ji})})]))])]}))(),Dt=fv(fv(et)(k))(E8(_));return t(fv(Dt)(u.startStop.start(void 0)))(),u.startStop.stop(Dt)()}}),Vm(a.startStop.stop)(function(A){return fv(A)(fv(t(HE(void 0)))(u.startStop.start(void 0)))})])])([Ve(yy([Jl(o)("Turn on"),Jl(a.startStop.stop)("Turn off")]))]),sr([WE(Lf(M8(Mt.value)))([Jl(a.startStop.stop)("display:block;"),Jl(o)("display:none;")])])(O8(function(A){return yi([yy([zm(w8(di.value)("checkbox")),zm(qE(ee.value)(Vr(M(A(void 0))))),Jl(o)(I8(dy.value)("false"))])])([])})(P8([function(A){return A.cbx0},function(A){return A.cbx1},function(A){return A.cbx2},function(A){return A.cbx3}])(u.cbx)))])}})})}}}};var KE={recip:function(t){return 1/t},Ring0:function(){return jf}};var YE=function(t){return function(r){return{EuclideanRing0:function(){return t},DivisionRing1:function(){return r}}}};function jl(t){return function(){return function(r){return t(r)()}}}function Xl(t){return function(r){return function(e){return function(n){return function(){return n.addEventListener(t,r,e)}}}}}function Ql(t){return function(r){return function(e){return function(n){return function(){return n.removeEventListener(t,r,e)}}}}}function gy(t){return t.clientX}function hy(t){return t.clientY}function cv(t){return t.button}var lv=At("MouseEvent");var ZE=g(L),Gm=Dn(Bt)(Me),Rf=po();var H8=sS(Te),z8=Yv(Te);var tM=function(t){return function(r){return Cn(function(e){return we(r)(function(n){return function(){var a=Ur(t.buttons)();return e({value:n,buttons:a})()}})})}};var rM=function(){var r=Or(w.value)(),e=Or(Mb)(),n=ZE(JA)(Fo)(),u=jl(function(f){return Gm(function(m){return te(new $({x:gy(m),y:hy(m)}))(r)})(lv(f))})(),a=jl(function(f){return Gm(function(m){return Yf(H8(cv(m)))(e)})(lv(f))})(),o=jl(function(f){return Gm(function(m){return Yf(z8(cv(m)))(e)})(lv(f))})();Xl(Rf("mousemove"))(u)(!1)(n)(),Xl(Rf("mousedown"))(a)(!1)(n)(),Xl(Rf("mouseup"))(o)(!1)(n)();var i=function(){return Ql(Rf("mousemove"))(u)(!1)(n)(),Ql(Rf("mousedown"))(a)(!1)(n)(),Ql(Rf("mouseup"))(o)(!1)(n)()};return{position:r,buttons:e,dispose:i}},eM=Cn(function(t){return function(){var e=ZE(JA)(Fo)(),n=jl(function(u){return Gm(function(a){return t(cv(a))})(lv(u))})();return Xl(Rf("mousedown"))(n)(!1)(e)(),Ql(Rf("mousedown"))(n)(!1)(e)}});var V8=g(ft);var uM=function(t){return Bu(function(r){return V8(function(e){return e.value(e.buttons)})(tM(t)(r))})};var Cy=function(t){return t};function Jm(){return Date.now()}var EM=function(t){return Cn(function(r){return we(t)(function(e){return function(){var u=Jm();return r({time:u,value:e})()}})})};var g6=g(ft),h6=Bu(function(t){return g6(function(r){return r.value(r.time)})(EM(t))}),Ty=g(Vl(ft))(function(){var t=vl(_C);return function(r){return t(Cy(r))}}())(h6);var Oy=Q(Z),x6=Ao(me),C6=Xc(me),$6=Bm(me),RM=Wm(ft),T6=YE(Ev)(KE),Fy=g(Vl(ft)),MM=ie(),OM=at(wE(ft)),F6=OE(me),Zm=B(ft),wM=W(),E6=mt({reflectType:function(){return`<section>
  <h2>Fix</h2>

  <p>Fix, like it's equivalent in ocarina that we've already seen, creates a feedback loop. However, in this case, we are talking about a feedback loop of <i>events</i>, not sound.</p>

  <p>At first glance, it may not be clear why we need an event stream to feed back into itself? It seems prone to saturation: if you have a counter that feeds back into itself with a delay, after a few seconds you'll have so many events that it will crash your browser (I've tried it!).</p>

  <p>However, there's one important circumstance where you need fixed points: when an event can only be defined in terms of itself. One classic category of this is the <i>differential equation</i>. Differential equations allow you to produce <a href="https://en.wikipedia.org/wiki/Simple_harmonic_motion">Slinky effects, aka simple harmonic motion,</a> and a lot of other neat behaviors that are difficult to produce via other means.</p>

  <p>Let's listen to the sound of simple harmonic motion in the example below, courtesy of <code>fix</code>. The differential equation in the example below comes from Phil Freeman, the creator of the PureScript language and the author of the <code>purescript-behaviors</code> package. When you click "Turn on", you won't hear much, but press and release your mouse anywhere on the screen to hear the differential equation take flight!</p>

  <pre><code>~txt~</code></pre>

  ~empl~

  <p>When working with stateful events, a good way to decide if you should use <code>fold</code> versus <code>fix</code> is to ask the following question: can I incrementally change my state based on an initial state, or is my state defined in terms of how it changes? If you can incrementally change your state, go with <code>fold</code>. If, on the other hand, your state is defined in terms of how it changes, go with <code>fix</code>.</p>
</section>`}})()()(wM(wM(R)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}})),M6=Pi()(De({reflectSymbol:function(){return"start"}})()()(De({reflectSymbol:function(){return"stop"}})()()(Yn)()())()()),Kl=gt(Wt),IM=F(j),O6=_u(qt)(Dr),Ua=g(ft),w6=st(ke),PM=oe(ft),I6=at(pn),LM=F(Bt),P6=Mn(ne),L6=Na(ne),Ey=g(x_),R6=sc(F$),jm=ct(Js),N6=F(C_),U6=Ie(Se(fi)()(ta)()(Io)),B6=Ie(Se(Mo)()(ra)()(ta)),W6=Ie(Se(Oo)()(ea)()(ra)),q6=Ie(Se(wo)()(Po)()(ea)),Xm=at(js),pv=mu(pt),_v=Du()(Ia),Uo=xl(Af),vv=Xo(za),wy=Cr(),Iy=kt(xr),NM={reflectSymbol:function(){return"q"}},Py={reflectSymbol:function(){return"frequency"}},Ly=Sr()(),H6=Rl(Ok(Zt(wy(kt(Iy(vT)()()()(NM))(dk)()()()(Py)))(Ly))),z6=ZT(Tl),My=Xi(ki(Zt(wy(kt(Iy(ji)()()()(NM))(bi)()()()(Py)))(Ly))),Qm=Pl(Ol(Zt(wy(kt(Iy(Ml($l(fi)))()()()({reflectSymbol:function(){return"spec"}}))(Fl)()()()(Py)))(Ly))),Km=X(),Ym=wf()(Ia),sv=rr(or),V6=mn(ne),G6=rn(qt)(Dr),J6=function(t){var r=function(o){var i=o.Filterable1().Functor1(),f=B(i),m=rs(o),v=No(i),D=da(o),A=Ao(o);return function(b){var _=b.DivisionRing1().Ring0(),k=_.Semiring0(),V=Er(k),et=Pn(k),Dt=Ja(b.EuclideanRing0()),vr=Ye(k),Gr=du(_);return function(H){var Ht=Er(H);return function(vt){return function(qr){return function(Wr){return function(fn){var On=V(et)(et),ia=function(be){return function(ue){if(be.last instanceof w)return ue;if(be.last instanceof $)return Ht(ue)(vt(function(Wu){return Dt(vr(Wu(Ht(be.last.value0.value1)(be.now.value1)))(Gr(be.now.value0)(be.last.value0.value0)))(On)}));throw new Error("Failed pattern match at Ocarina.Example.Docs.FixEx (line 106, column 5 - line 106, column 35): "+[be.constructor.name,ue.constructor.name])}};return Bu(function(be){var ue=ov(fn)(f(be)(Oy)),Wu=m(v(N.create)(Wr)(ue)),to=D(Gt(ia))(qr)(Wu);return A(to)(be)})}}}}}}},e=function(o){var i=r(o);return function(f){return i(f)(f.DivisionRing1().Ring0().Semiring0())(function(m){return m(Oy)})}},n=e(me)(T6),u=function(o){return function(i){return Bu(function(f){return x6(C6(function(m){var v=i($6(o)(m));return RM(v)(f)}))(f)})}},a=function(o){return function(i){return function(f){if(vS(o))return-8*(i-1)-f*2;if(Jr)return 2*(4-i);throw new Error("Failed pattern match at Ocarina.Example.Docs.FixEx (line 62, column 3 - line 64, column 34): "+[o.constructor.name,i.constructor.name,f.constructor.name])}}};return u(2)(function(o){return n(2)(Fy(MM)(Ty))(function(){var i=u(10)(function(f){return n(10)(Fy(MM)(Ty))(OM(OM(Fy(a)(uM(t)))(o))(f))});return F6(i)(Zm(eM)(i))}())})},j6=function(){return d.value}(),UM=function(t){return function(r){return function(e){return function(n){return E6(j6)({txt:S(`module Main

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
  )`),empl:M6(d.value)(function(u){return function(a){var o=Kl(IM(void 0))(a.start);return Rr([on([O6(Ua(function(){var i=w6(ee.value);return function(f){return i(Vr(M(f)))}}()))([PM(I6(Zm(o)(Oy))(Kl(IM(LM(void 0)))(Ua(function(i){return i.value0})(n))))(function(i){return function(){i();var m=P6(),v=L6(m)(),D=rM(),A=y_(0)(1e4)(),b=function(H){return{o:H.value0+.04,n:H.value1,t:Ji}},_=Ey(function(H){return H-.5})(R6),k=jm(_)(function(H){return jm(_)(function(Ht){return jm(_)(function(vt){return jm(_)(function(qr){return N6(U6(H)(B6(Ht)(W6(vt)(q6(qr)(Oa)))))})})})}),V=Xm(Ey(N.create)(k))(k),et=Xm(Xm(Xm(Ey(function(H){return function(Ht){return function(vt){return function(qr){return{s0:H,s1:Ht,s2:vt,s3:qr}}}}})(V))(V))(V))(V),Dt=bf(et)({newSeed:Df(A),size:5}),vr=Ul(m)(ti(Ua(function(H){return new N(H.acTime,H.value)})(Gl(m)(RM(J6(D))(xf))))(function(H){return[pv(0)(Ua(function(){var Ht=Uo(function(vt){return vv(-.4)(.5*(vt-1))});return function(vt){return _v(b(Ht(vt)))}}())(H))([H6({frequency:90.4,q:20})([z6(90.4)])]),pv(0)(Ua(function(){var Ht=Uo(function(vt){return vv(-.2)(.4*(vt-3))});return function(vt){return _v(b(Ht(vt)))}}())(H))([My({frequency:90.4*4,q:20})([Qm({frequency:90.4*3.02,spec:Dt.s0})(Kl(Km)(Ua(function(){var Ht=Uo(function(vt){return 273.00800000000004+14*(vt-1)});return function(vt){return Ym(b(Ht(vt)))}}())(H)))])]),pv(0)(Ua(function(){var Ht=Uo(function(vt){return vv(-.1)(.2*(vt-6))});return function(vt){return _v(b(Ht(vt)))}}())(H))([My({frequency:90.4*6,q:20})([Qm({frequency:90.4*5.07,spec:Dt.s1})(Kl(Km)(Ua(function(){var Ht=Uo(function(vt){return 458.32800000000003+18*(vt-1)});return function(vt){return Ym(b(Ht(vt)))}}())(H)))])]),pv(0)(Ua(function(){var Ht=Uo(function(vt){return vv(0)(.2*(vt-3))});return function(vt){return _v(b(Ht(vt)))}}())(H))([My({frequency:90.4*8,q:20})([Qm({frequency:90.4*7.13,spec:Dt.s2})(Kl(Km)(Ua(function(){var Ht=Uo(function(vt){return 644.552+32*(vt-1)});return function(vt){return Ym(b(Ht(vt)))}}())(H)))])]),pv(0)(Ua(function(){var Ht=Uo(function(vt){return vv(0)(.1*(vt-7))});return function(vt){return _v(b(Ht(vt)))}}())(H))([Qm({frequency:90.4*9.14,spec:Dt.s3})(Kl(Km)(Ua(function(){var Ht=Uo(function(vt){return 826.2560000000001+31*(vt-1)});return function(vt){return Ym(b(Ht(vt)))}}())(H)))])]}))(),Gr=sv(sv(vr)(v))(V6(m));return t(sv(Gr)(u.start(void 0)))(),u.stop(Gr)()}}),PM(a.stop)(function(i){return sv(i)(sv(t(LM(void 0)))(u.start(void 0)))})])])([Ve(G6([Zm(o)("Turn on"),Zm(a.stop)("Turn off")]))])])}})})}}}};var BM=W(),Q6=mt({reflectType:function(){return`<div>
  <h1>State</h1>

  <h3>Or Events 2.0</h3>
  <p>
    The name of this section is a bit of a nisnomer. While it will address the issue of maintaining state in an audio graph, it's really just about two mechanisms you can use to make an <code>Event</code> stateful. One is called <code>fold</code>, and the other is called <code>fix</code>. Both are part of the <code>IsEvent</code> typeclass, which means you get them for free when working with events.
  </p>

  ~fold~
  ~fix~

  <h2>Next steps</h2>
  <p>Using <code>fold</code> and <code>fix</code>, we can create internal state in our Web Audio works that would be really tedious and error-prone to achieve in vanilla JS or other compile-to-JS languages. There's still one nagging issue that we haven't addressed, though. For all of the flexibility we can achieve with events, we still can't flex the audio graph itself, meaning that we can't add or remove components. In the next section, we'll learn how to do that with <a ~next~ style="cursor:pointer;">subgraphs</a>.</p>
</div>`}})()()(BM(BM(Je()(R)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"fold"}})({reflectSymbol:function(){return"fold"}}))({reflectType:function(){return"fix"}})({reflectSymbol:function(){return"fix"}})),K6=ou(Wt)(j),Y6=rr(or),Z6=function(){return d.value}(),WM=function(t){return function(r){return function(e){return function(n){var u=fu(t)(e);return Q6(Z6)({next:K6(n)(Y6(r(z_.value))(Xe)),fold:QE(u)(r)(e)(n),fix:UM(u)(r)(e)(n)})}}}};var JM=Ju(Wn),Uy=gt(Wt),By=F(j),rJ=Du()(En),eJ=mu(pt),nJ=gc(gf),qM=W(),uJ=Yt({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(qM(qM(R)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}})),HM=oe(ft),aJ=at(pn),zM=g(ft),VM=F(Bt),iJ=st(ke),Ry=rr(or),oJ=wt(pt),fJ=qu(hi),cJ=g(ir),lJ=function(){function t(){}return t.value=new t,t}(),GM=function(){function t(){}return t.value=new t,t}(),Ny=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),pJ=`module Main where

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
`;var _J=function(){return d.value}(),vJ=function(t){var r=F(t);return function(e){var n=Cc(e)(kc);return function(u){return r(n({x:Ak,o:u}))}}},sJ=vJ(j)(),mJ=function(t){var r=F(t);return function(e){var n=Cc(e)(kc);return function(u){return r(n({x:J0,o:u}))}}},DJ=mJ(j)(),bJ=JM(Br)(function(t){var r=function(u){return Uy(sJ(u+.27*(t*vf(1.005)(t))))(DJ(u+3+.3*(t*vf(1.005)(t))))},e=function(u){return By(rJ({p:[0,.4,.1,.05,.01,0],o:u+.3*(t*vf(1.005)(t)),d:.8}))},n=function(u){return function(a){return eJ(0)(e(u))([nJ(200+t*a)(r(u))])}};return[n(.2)(4),n(.3)(6),n(.45)(14),n(.7)(20)]}),jM=function(t){return function(r){return function(e){return uJ(d.value)(_J)({txt:S(pJ),ex0:Be(function(n){return JM(function(u){return Uy(By(lJ.value))(u)})(function(u){return Rr([on([HM(aJ(zM(N.create)(u))(Uy(By(VM(void 0)))(zM(function(a){return a.value0})(e))))(function(a){return iJ(ee.value)(Vr(M(function(){return a.value0 instanceof Ny?Ry(Ry(a.value0.value0)(n(GM.value)))(t(VM(void 0))):function(){a.value1();var i=Im([oJ(1)(fJ(cJ(bJ)(Ke(0)(100))))])();return t(Ry(i)(n(GM.value)))(),n(new Ny(i))()}}())))})])([Ve(HM(u)(function(a){return a instanceof Ny?"Turn off":"Turn on"}))])])})})})}}};var Bo=function(){function t(){}return t.value=new t,t}();var Nf={attr:function(t){return function(r){return c({key:"max",value:s(r)})}}};var Wo=function(){function t(){}return t.value=new t,t}();var Uf={attr:function(t){return function(r){return c({key:"min",value:s(r)})}}};var qo=function(){function t(){}return t.value=new t,t}();var Bf={attr:function(t){return function(r){return c({key:"input",value:G(r)})}}};var Ho=function(){function t(){}return t.value=new t,t}();var Wf={attr:function(t){return function(r){return c({key:"step",value:s(r)})}}};var zo=function(){function t(){}return t.value=new t,t}();var qf={attr:function(t){return function(r){return c({key:"value",value:s(r)})}}};var aD=function(t){var r=gt(t);return function(e){return function(n){return r(e)(n(void 0))}}};var kJ=qS,dJ=K(kJ),Yl={convert:function(t){return t}},iD={convert:function(t){return t_(t)}},QM=function(t){return t},Wy=function(t){return t.convert},Tc=function(t){var r=Wy(t);return function(e){return function(n){return dJ(t_(e))(r(n(void 0)))}}};var yJ=_u(HS),oD=function(t){var r=Wy(t);return function(e){var n=yJ(e);return function(u){return function(a){return n(u)(QM(r(a)))}}}};function YM(t){return t.target}var Zl=function(t){return We(YM(t))};var zy=W(),SJ=Yt({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(zy(zy(zy(R)({reflectType:function(){return"wagtxt"}})({reflectSymbol:function(){return"wagtxt"}}))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})),xJ=Pi()(Mi({reflectSymbol:function(){return"slider"}})()()()(De({reflectSymbol:function(){return"s0"}})()()(De({reflectSymbol:function(){return"s1"}})()()(De({reflectSymbol:function(){return"s2"}})()()(Yn)()())()())()())(Mi({reflectSymbol:function(){return"startStop"}})()()()(De({reflectSymbol:function(){return"loading"}})()()(De({reflectSymbol:function(){return"start"}})()()(De({reflectSymbol:function(){return"stop"}})()()(Yn)()())()())()())(Yn)()())()()),Vy=gt(Wt),fD=F(j),CJ=tr(wl(Zt(Cr()(kt(kt(kt(kt(xr)(bm)()()()({reflectSymbol:function(){return"playbackRate"}}))(Dm)()()()({reflectSymbol:function(){return"loopStart"}}))(mm)()()()({reflectSymbol:function(){return"loopEnd"}}))(El)()()()({reflectSymbol:function(){return"buffer"}})))(Sr()()))),cD=aD(Wt),$J=X(),Hf=g(ft),TJ=Uu()(pm),FJ=RF(),EJ=NF(),ZM=at(pn),MJ=Er(Jn),OJ=K(Ne),wJ=g(ir),tO=oD(Yl)(Dr),mv=Tc(Yl),IJ=st(Zi),PJ=st(Uf),LJ=st(Nf),RJ=st(Wf),rO=Tc(iD),NJ=st(qf),UJ=st(Bf),BJ=Dn(Bt)(Me),WJ=Oc(Qn),qJ=cn(Nn),HJ=st(ke),eO=B(ft),Gy=F(Bt),nO=oe(ft),lD=rr(or),zJ=Q(Z),Jy=ct(qn),VJ=Mn(Ge),GJ=Na(Ge),JJ=de(Ge),jJ=mn(ne),XJ=`module Main where

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
`,QJ=function(){return d.value}(),KJ="https://freesound.org/data/previews/100/100981_1234256-lq.mp3",uO=function(t){return function(r){return function(e){return SJ(d.value)(QJ)({wagtxt:S(`run2_
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
          (add <$> (pure 0.0 <|> sl1))`),txt:S(XJ),ex1:xJ(d.value)(function(n){return function(u){var a=Vy(u.startStop.start)(fD(void 0)),o=function(i){return CJ({buffer:i,playbackRate:2.6,loopStart:.6,loopEnd:1.1})(cD($J)(function(){return cD(Hf(function(){var f=cu(0)(.2)(100)(5);return function(m){return TJ(f(m))}}())(u.slider.s0))(function(){return cD(Hf(function(){var f=cu(0)(0)(100)(1.2);return function(m){return FJ(f(m))}}())(u.slider.s1))(function(){return Hf(function(){var f=cu(0)(.05)(100)(1);return function(m){return EJ(f(m))}}())(function(f){return ZM(f)(u.slider.s2)}(Hf(MJ)(Vy(fD(0))(u.slider.s1))))})})}))};return Rr(OJ(wJ(function(i){return Rr([S(i.l),yi([tO(fD)(mv(IJ(di.value)("range"))(function(){return mv(PJ(Wo.value)("0"))(function(){return mv(LJ(Bo.value)("100"))(function(){return mv(RJ(Ho.value)("1"))(function(){return rO(NJ(zo.value)("50"))(function(){return UJ(qo.value)(Vr(function(){var f=BJ(WJ(Dc)(i.f)),m=qJ(yl);return function(v){return f(m(Zl(v)))}}()))})})})})}))])([])])})([{l:"Playback rate",f:n.slider.s0},{l:"Loop start",f:n.slider.s1},{l:"Loop end",f:n.slider.s2}]))([on([tO(Hf(function(){var i=HJ(ee.value);return function(f){return i(Vr(M(f)))}}()))(mv(eO(u.startStop.loading)(Gy(void 0)))(function(){return rO(nO(u.startStop.stop)(function(i){return lD(i)(lD(t(Gy(void 0)))(n.startStop.start(void 0)))}))(function(){return nO(ZM(eO(a)(zJ))(Vy(fD(Gy(void 0)))(Hf(function(i){return i.value0})(e))))(function(i){return function(){i(),n.startStop.loading(void 0)();var m=Ca(Jy(VJ)(function(v){return Jy(GJ(v))(function(D){return Jy(ut(v)(KJ))(function(A){return JJ(function(){var _=Y(v)([o(A)])(),k=lD(lD(_)(D))(jJ(v));return n.startStop.stop(k)(),k})})})}))();return t(function(){return n.startStop.start(void 0)(),ii(Lo(m))()})(),void 0}})})}))])([Ve(cD(Hf(M("Turn off"))(u.startStop.stop))(function(){return Hf(M("Turn on"))(a)}))])]))}})})}}};var aO=W(),ZJ=Yt({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(aO(aO(R)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}})),tj=Pi()(De({reflectSymbol:function(){return"slider"}})()()(Mi({reflectSymbol:function(){return"startStop"}})()()()(De({reflectSymbol:function(){return"start"}})()()(De({reflectSymbol:function(){return"stop"}})()()(Yn)()())()())(Yn)()())()()),jy=gt(Wt),Xy=F(j),Ba=g(ft),rj=Er(Jn),ej=X(),nj=wf()(pm),uj=Sm(vm),aj=wt(pt),Qy=mu(pt),Ky=Du()(En),lO=Cr(),pO=kt(xr),_O={reflectSymbol:function(){return"q"}},vO={reflectSymbol:function(){return"frequency"}},sO=Sr()(),iO=Xi(ki(Zt(lO(kt(pO(ji)()()()(_O))(bi)()()()(vO)))(sO))),ij=Ll(wk(Zt(lO(kt(pO(DT)()()()(_O))(yk)()()()(vO)))(sO))),oj=oD(Yl)(Dr),pD=Tc(Yl),fj=st(Zi),cj=st(Uf),lj=st(Nf),pj=st(Wf),_j=Tc(iD),vj=st(qf),sj=st(Bf),mj=Dn(Bt)(Me),Dj=Oc(Qn),bj=cn(Nn),Aj=_u(qt)(Dr),kj=st(ke),oO=oe(ft),dj=at(pn),Yy=B(ft),yj=Q(Z),fO=F(Bt),gj=Mn(ne),hj=No(ft),Dv=rr(or),cO=mn(ne),Sj=rn(qt)(Dr),xj=`module Main where

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
  )`,Cj=Bu(function(t){return Cn(function(r){return we(t)(function(e){return function(){var u=Ui();return r(e(u))()}})})}),$j=function(){return d.value}(),Tj=function(t){if(t<.142857)return 261.625565;if(t<.285714)return 293.664768;if(t<.428571)return 349.228231;if(t<.571429)return 391.995436;if(t<.714286)return 440;if(t<.857143)return 523.251131;if(Jr)return 587.329536;throw new Error("Failed pattern match at Ocarina.Example.Docs.Events.Ex2 (line 224, column 1 - line 224, column 23): "+[t.constructor.name])},mO=function(t){return function(r){return function(e){return ZJ(d.value)($j)({txt:S(xj),ex2:tj(d.value)(function(n){return function(u){var a=jy(u.startStop.start)(Xy(void 0)),o=function(i){return ti(i)(function(f){var m=Ba(function(){var k=rj(.01);return function(V){return k(Ue(V))}}())(f),v=Ba(pu)(f),D=jy(ej)(Ba(function(k){return nj(Tj(k))})(v)),A=Ba(function(k){return cm(function(V){return{p:[0,.15,.05,.01,.005,5e-4,0],d:.4,o:V}}(k))})(m),b=Ba(function(k){return cm(function(V){return{p:[0,.3,.1,.05,.01,.005,0],d:.4,o:V}}(k))})(m),_=Ba(function(k){return cm(function(V){return{p:[0,.6,.2,.1,.5,.03,0],d:.4,o:V}}(k))})(m);return[iu(uj(0)(D))(function(k){return aj(2)([Qy(0)(Ba(Ky)(_))([iO({frequency:1e3,q:20})([k])]),Qy(0)(Ba(Ky)(b))([iO({frequency:2e3,q:20})([k])]),Qy(0)(Ba(Ky)(A))([ij({frequency:4e3,q:20})([k])])])})]})};return Rr([Rr([S("tempo"),yi([oj(Xy)(pD(fj(di.value)("range"))(function(){return pD(cj(Wo.value)("0"))(function(){return pD(lj(Bo.value)("100"))(function(){return pD(pj(Ho.value)("1"))(function(){return _j(vj(zo.value)("50"))(function(){return sj(qo.value)(Vr(function(){var i=mj(Dj(Dc)(n.slider)),f=bj(yl);return function(m){return i(f(Zl(m)))}}()))})})})})}))])([])]),on([Aj(Ba(function(){var i=kj(ee.value);return function(f){return i(Vr(M(f)))}}()))([oO(dj(Yy(a)(yj))(jy(Xy(fO(void 0)))(Ba(function(i){return i.value0})(e))))(function(i){return function(){i();var m=gj(),v=hj(N.create)(Cj)(NE(m)(.91)(Ba(cu(0)(.42)(100)(1.4))(u.slider))),D=Ul(m)(o(v))(),A=Dv(D)(cO(m));return t(Dv(A)(n.startStop.start(void 0)))(),n.startStop.stop(Dv(A)(cO(m)))()}}),oO(u.startStop.stop)(function(i){return Dv(i)(Dv(t(fO(void 0)))(n.startStop.start(void 0)))})])])([Ve(Sj([Yy(a)("Turn on"),Yy(u.startStop.stop)("Turn off")]))])])}})})}}};var Ej=function(){return d.value}(),DO=function(){return Yt({reflectType:function(){return`<section>
  <h2>Three flavors of events.</h2>

  <p>When we're in the browser, events tend to come in three broad categories:</p>

  <ul>
    <li>Things that need to happen <span style="font-weight: 800;">now</span>.</li>
    <li>Things that happen as the result of a user interaction.</li>
    <li>Things that are scheduled to happen in the future, for example with <code>setTimeout</code>.</li>
  </ul>

  <p>The next three examples cover all three cases.</p>

</section>`}})({reflectType:function(){return"@"}})()()(R)(d.value)(Ej)({})}();var Oj=function(){return d.value}(),bO=function(){return Yt({reflectType:function(){return`<section>
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
</section>`}})({reflectType:function(){return"@"}})()()(R)(d.value)(Oj)({})}();var Ij=function(){return d.value}(),AO=function(){return Yt({reflectType:function(){return`<section>

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
</section>`}})({reflectType:function(){return"@"}})()()(R)(d.value)(Ij)({})}();var Lj=ou(Wt)(j),Rj=rr(or),tp=W(),Nj=Yt({reflectType:function(){return`<div>
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
</div>`}})({reflectType:function(){return"@"}})()()(tp(tp(tp(tp(tp(Je()(tp(R)({reflectType:function(){return"primer"}})({reflectSymbol:function(){return"primer"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"inOcarina"}})({reflectSymbol:function(){return"inOcarina"}}))({reflectType:function(){return"flavors"}})({reflectSymbol:function(){return"flavors"}}))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}}));var Uj=function(){return d.value}(),kO=function(t){return function(r){return function(e){return function(n){var u=function(o){return Lj(n)(Rj(r(o))(Xe))},a=fu(t)(e);return Nj(d.value)(Uj)({next:u(q_.value),primer:AO,inOcarina:bO,flavors:DO,ex0:jM(a)(r)(n),ex1:uO(a)(r)(n),ex2:mO(a)(r)(n)})}}}};var Wj=Yt({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(W()(R)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),qj=ai(Ku),Zy=at(D_),Hj=g(ml),_D=Qu(Ku),zj=wt(pt),Vj=F(j),Gj=Cc()(kc),Jj=lm(),jj=Er(Jn),vD=au(Pa),Xj=function(){return d.value}(),dO=function(t){return function(r){return function(e){return Wj(d.value)(Xj)({ai0:_t(e)(t)(function(n){return qj(Zy(Zy(Zy(Hj(function(u){return function(a){return function(o){return function(i){return{tink0:u,tink1:a,tink2:o,tink3:i}}}}})(_D(ut(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(_D(ut(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(_D(ut(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(_D(ut(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(u){return Y(n)([zj(1)(function(){var a=function(o){return Vj(Gj(Jj(jj(o))(E_)))};return[vD(u.tink0)(a(.1)),vD(u.tink1)(a(.2)),vD(u.tink2)(a(.9)),vD(u.tink3)(a(1.8))]}())])}})})}}};var Kj=Yt({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(W()(R)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),Yj=ai(Ku),tg=at(D_),Zj=g(ml),sD=Qu(Ku),t7=wt(pt),r7=F(j),e7=Cc()(kc),n7=lm(),u7=Er(Jn),a7=hu(Ga),i7=oe(ir),o7=au(Pa),f7=function(){return d.value}(),yO=function(t){return function(r){return function(e){return Kj(d.value)(f7)({ai0:_t(e)(t)(function(n){return Yj(tg(tg(tg(Zj(function(u){return function(a){return function(o){return function(i){return{tink0:u,tink1:a,tink2:o,tink3:i}}}}})(sD(ut(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(sD(ut(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(sD(ut(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(sD(ut(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(u){return Y(n)([t7(1)(function(){var a=function(i){return r7(e7(n7(u7(i))(E_)))},o=function(i){var f=a7(i)(4);return f===0?u.tink0:f===1?u.tink1:f===2?u.tink2:u.tink3};return i7(Ke(0)(100))(function(i){var f=Br(i);return o7(o(i))(a(.3+.3*(f*vf(1.005)(f))))})}())])}})})}}};var l7=Yt({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(W()(R)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),p7=tr(cr),_7=X(),v7=wt(pt),bv=Xi(ki(Zt(Cr()(kt(kt(xr)(ji)()()()({reflectSymbol:function(){return"q"}}))(bi)()()()({reflectSymbol:function(){return"frequency"}})))(Sr()()))),s7=function(){return d.value}(),gO=function(t){return function(r){return function(e){return l7(d.value)(s7)({ai0:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([iu(p7(u)(_7))(function(a){return v7(.8)([bv({frequency:400,q:1})([a]),bv({frequency:880,q:5})([a]),bv({frequency:1200,q:10})([a]),bv({frequency:2e3,q:20})([a]),bv({frequency:3e3,q:30})([a])])})])}})})}}};var D7=Yt({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(W()(R)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),b7=tr(cr),A7=X(),k7=wt(pt),d7=oe(ir),y7=Ju(Wn),g7=Xi(ki(Zt(Cr()(kt(kt(xr)(ji)()()()({reflectSymbol:function(){return"q"}}))(bi)()()()({reflectSymbol:function(){return"frequency"}})))(Sr()()))),h7=function(){return d.value}(),hO=function(t){return function(r){return function(e){return D7(d.value)(h7)({ai0:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([iu(b7(u)(A7))(function(a){return k7(.8)(d7(Ke(0)(40))(y7(Br)(function(o){return g7({frequency:200+o*150,q:30})([a])})))})])}})})}}};var x7=Yt({reflectType:function(){return`<div>
  <pre><code>\\buf -> run2_
  [ fix
      \\b -> gain_ 1.0
        [ playBuf buf bangOn
        , delay_ 0.1 [ gain_ 0.6 [ b ] ]
        ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(W()(R)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),SO=wt(pt),C7=au(Pa),$7=X(),T7=hf(Ai),F7=function(){return d.value}(),xO=function(t){return function(r){return function(e){return x7(d.value)(F7)({ai0:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(u){return Y(n)([wu(function(a){return SO(1)([C7(u)($7),T7(.1)([SO(.6)([a])])])})])}})})}}};var CO=W(),M7=Yt({reflectType:function(){return`<div>
  <pre><code>@txt@</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(CO(CO(R)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),O7=au(Pa),w7=X(),$O=wt(pt),TO=mu(pt),I7=function(){return d.value}(),P7=function(t){var r=F(t);return function(e){return r(Du(e)(En)({p:[1,1,0],o:0,d:10}))}},L7=P7(j)(),R7=function(t){var r=F(t);return function(e){return r(Du(e)(En)({p:[1,1,0],o:0,d:8}))}},N7=R7(j)(),U7=function(t){var r=hf(t);return function(e){var n=wt(e);return function(u){var a=Ll(u);return function(o){return function(i){return function(f){return function(m){return r(o)([n(i)([a(f)(m)])])}}}}}}},rp=U7(Ai)(pt)(Il),FO=function(t){return function(r){return function(e){return M7(d.value)(I7)({txt:S(`dgh d g h i =
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
  ]`),ai0:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(u){return Y(n)([iu(O7(u)(w7))(function(a){return wu(function(o){return $O(1)([a,rp(.15)(.7)(1500)([wu(function(i){return TO(1)(L7)([rp(.4)(.5)(2500)([o,i])])})]),rp(.29)(.85)(2e3)([wu(function(i){return $O(1)([rp(.6)(.6)(3500)([o,wu(function(f){return TO(1)(N7)([rp(.75)(.6)(4e3)([i,f]),rp(.75)(.55)(3e3)([a])])})])])})])])})})])}})})}}};var W7=ou(Wt)(j),q7=rr(or),H7=Yt({reflectType:function(){return`<section>
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
  </section>`}})({reflectType:function(){return"@"}})()()(Je()(R)({reflectType:function(){return"hwLink"}})({reflectSymbol:function(){return"hwLink"}}));var z7=function(){return d.value}(),EO=function(t){return function(r){return function(e){return function(n){var u=function(a){return W7(n)(q7(r(a))(Xe))};return H7(d.value)(z7)({hwLink:u(hc.value)})}}}};var G7=ou(Wt)(j),J7=rr(or),Fc=W(),j7=Yt({reflectType:function(){return`<div>
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
</div>`}})({reflectType:function(){return"@"}})()()(Fc(Fc(Fc(Fc(Fc(Fc(Fc(Je()(R)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"intro"}})({reflectSymbol:function(){return"intro"}}))({reflectType:function(){return"code5"}})({reflectSymbol:function(){return"code5"}}))({reflectType:function(){return"code4"}})({reflectSymbol:function(){return"code4"}}))({reflectType:function(){return"code3"}})({reflectSymbol:function(){return"code3"}}))({reflectType:function(){return"code2"}})({reflectSymbol:function(){return"code2"}}))({reflectType:function(){return"code1"}})({reflectSymbol:function(){return"code1"}}))({reflectType:function(){return"code0"}})({reflectSymbol:function(){return"code0"}}));var X7=function(){return d.value}(),MO=function(t){return function(r){return function(e){return function(n){var u=function(o){return G7(n)(J7(r(o))(Xe))},a=fu(t)(e);return j7(d.value)(X7)({intro:EO(t)(r)(e)(n),next:u(B_.value),code0:dO(a)(r)(n),code1:yO(a)(r)(n),code2:gO(a)(r)(n),code3:hO(a)(r)(n),code4:xO(a)(r)(n),code5:FO(a)(r)(n)})}}}};var OO=E("code"),rg=OO(h(O));var wO=E("pre"),eg=wO(h(O));var Z7=rr(or),IO=W(),tX=Yt({reflectType:function(){return`<div>
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
</div>`}})({reflectType:function(){return"@"}})()()(IO(Je()(IO(R)({reflectType:function(){return"result"}})({reflectSymbol:function(){return"result"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}})),rX=F(ru),eX=wt(pt),nX=gc(gf),uX=X(),aX=ou(Wt)(j),iX=function(){return d.value}(),PO=function(t){return function(r){return function(e){return function(n){var u=Z7(r(W_.value))(Xe),a=fu(t)(e);return tX(d.value)(iX)({code:eg([rg([S(`case e of
  Just x -> x *> push Nothing
  _ -> (run2_ [ gain_ 0.15 [ sinOsc 440.0 bangOn ] ]
         >>= Just >>> push`)])]),result:_t(n)(a)(function(o){return rX(void 0)})(function(o){return function(i){return Y(o)([eX(.15)([nX(440)(uX)])])}}),next:aX(n)(u)})}}}};var LO=ul;var RO=function(){return function(t){return t}};var NO=function(){return function(t){return t}};var ng=function(){function t(){}return t.value=new t,t}();var UO={attr:function(t){return function(r){return c({key:"height",value:s(r)})}}};var ug=function(){function t(){}return t.value=new t,t}();var BO={attr:function(t){return function(r){return c({key:"width",value:s(r)})}}};var ag=E("canvas");var ig=function(){function t(){}return t.value=new t,t}();var WO={attr:function(t){return function(r){return c({key:"@self@",value:G(r)})}}};function mD(t){return function(){return t.getContext("2d")}}function Av(t){return function(r){return function(){t.fillStyle=r}}}function DD(t){return function(){t.beginPath()}}function bD(t){return function(){t.fill()}}function og(t){return function(r){return function(){t.arc(r.x,r.y,r.radius,r.start,r.end,r.useCounterClockwise)}}}function AD(t){return function(r){return function(){t.fillRect(r.x,r.y,r.width,r.height)}}}var XO=Ft(gu),SX=Yt({reflectType:function(){return"<section>@ex1@</section>"}})({reflectType:function(){return"@"}})()()(W()(R)({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})),xX=Pi()(De({reflectSymbol:function(){return"canvas"}})()()(De({reflectSymbol:function(){return"slider"}})()()(Mi({reflectSymbol:function(){return"startStop"}})()()()(De({reflectSymbol:function(){return"loading"}})()()(De({reflectSymbol:function(){return"start"}})()()(De({reflectSymbol:function(){return"stop"}})()()(Yn)()())()())()())(Yn)()())()())()()),kD=gt(Wt),kv=F(j),aa=g(ft),QO=Cr(),KO=kt(xr),YO=Sr()(),CX=hm(gm(Zt(QO(kt(KO(ym)()()()({reflectSymbol:function(){return"fftSize"}}))(dm)()()()({reflectSymbol:function(){return"cb"}})))(YO))),fg=F(Bt),$X=F(Ha),TX=au(Pa),FX=X(),EX=Uu()(Ia),ep=xl(Af),qO=wt(pt),MX=N_(Ik(Zt(QO(kt(KO(bT)()()()({reflectSymbol:function(){return"maxDelayTime"}}))(gk)()()()({reflectSymbol:function(){return"delayTime"}})))(YO))),dD=py()(Ia),cg=mu(pt),OX=Du()(Ia),zf=h(Dr),lg=_u(qt)(Dr),wX=st(BO),IX=st(UO),PX=st(qx),HO=st(WO),pg=Dn(Bt)(Me),LX=st(Zi),RX=st(Uf),NX=st(Nf),UX=st(Wf),BX=st(qf),WX=st(Wx),qX=st(Bf),HX=Oc(Qn),zX=cn(Nn),zO=rn(qt)(Dr),VX=st(ol),GX=st(ke),VO=B(ft),GO=oe(ft),dv=rr(or),JX=at(pn),jX=Q(Z),yD=ct(qn),XX=Mn(Ge),QX=Na(Ge),KX=g(To),YX=NO(),ZX=iC(Ku)(LO),t9=RO(),JO=de(Ge),r9=Yb(),e9=en(xi)(Bt),n9=$u(Bt)(Me),u9=g(L),a9=g(ir),i9=Lm(Pm),o9=mn(ne),f9=function(){return 2*sf}(),np=function(t){return{o:t.value0+.04,n:t.value1,t:Ji}};var c9=function(){return d.value}(),l9=function(t){var r=F(t);return function(e){var n=wf(e)(En);return function(u){return function(a){return r(n({p:[u,a],o:0,d:16}))}}}},up=l9(j)(),p9=function(t){var r=F(t);return function(e){return r(Du(e)(En)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:18}))}},_9=p9(j)(),v9=function(t){var r=F(t);return function(e){return r(Du(e)(En)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:24}))}},s9=v9(j)();var m9=function(t){var r=N_(t);return function(e){var n=mu(e);return function(u){var a=Uk(u);return function(o){return function(i){return function(f){return function(m){return function(v){return function(D){return function(A){return r(o)(i)([n(f)(m)([a(v)(D)(A)])])}}}}}}}}}},gD=m9(Ai)(pt)(Il),D9=function(t){var r=N_(t);return function(e){var n=mu(e);return function(u){var a=Nk(u);return function(o){return function(i){return function(f){return function(m){return function(v){return function(D){return function(A){return r(o)(i)([n(f)(m)([a(v)(D)(A)])])}}}}}}}}}},jO=D9(Ai)(pt)(qT),b9=function(t){var r=F(t);return function(e){var n=py(e)(En);return function(u){return function(a){return r(n({p:[u,a],o:0,d:16}))}}}},A9=b9(j)(),ZO=400,_g=Br(ZO),k9=function(){return XO(ZO)+"px"}(),tw=600,vg=Br(tw),d9=function(){return XO(tw)+"px"}(),y9={pluck0:"https://freesound.org/data/previews/493/493016_10350281-lq.mp3",pluck1:"https://freesound.org/data/previews/141/141524_2558140-lq.mp3",strum0:"https://freesound.org/data/previews/234/234738_3635427-lq.mp3"},rw=function(t){return function(r){return function(e){return SX(d.value)(c9)({ex1:xX(d.value)(function(n){return function(u){var a=kD(kv(void 0))(u.startStop.start),o=function(i){return function(f){return function(m){var v=aa(function(D){return new N(D.acTime,D.value)})(Gl(i)(u.slider));return[CX({cb:function(D){return function(){return te(new $(D))(m)(),te(w.value)(m)}},fftSize:om.value})($X(iu(TX(f)(kD(FX)(aa(function(){var D=ep(cu(0)(.96)(100)(1.04));return function(A){return EX(np(D(A)))}}())(v))))(function(D){return wu(function(A){return qO(1)([D,MX({maxDelayTime:2.5,delayTime:1})(aa(function(){var b=ep(cu(0)(.5)(100)(2.45));return function(_){return dD(np(b(_)))}}())(v))([cg(.4)(aa(function(){var b=ep(cu(0)(.6)(100)(.9));return function(_){return OX(np(b(_)))}}())(v))([D])]),gD(.15)(zf)(.7)(zf)(1500)(up(1500)(3e3))([wu(function(b){return cg(1)(_9)([gD(.4)(zf)(.5)(zf)(3e3)(up(3e3)(100))([A,b])])})]),gD(.29)(aa(function(){var b=ep(cu(0)(.1)(100)(.4));return function(_){return dD(np(b(_)))}}())(v))(.85)(zf)(2e3)(up(2e3)(5e3))([wu(function(b){return qO(1)([gD(.6)(aa(function(){var _=ep(cu(0)(.8)(100)(.3));return function(k){return dD(np(_(k)))}}())(v))(.6)(zf)(3500)(up(3500)(100))([A,wu(function(_){return cg(1)(s9)([jO(.75)(aa(function(){var k=ep(cu(0)(.9)(100)(.1));return function(V){return dD(np(k(V)))}}())(v))(.6)(zf)(4e3)(up(4e3)(200))([b,_]),jO(.75)(A9(.75)(.2))(.55)(zf)(200)(up(200)(4e3))([D])])})])])})])])})})))]}}};return Rr([ag([kD(lg(kv)([wX(ug.value)(d9),IX(ng.value)(k9),PX(Mt.value)("width: 100%;"),HO(ig.value)(function(){var i=pg(function(f){return function(){var v=mD(f)();return Av(v)("black")(),AD(v)({width:vg,height:_g,x:0,y:0})(),void 0}});return function(f){return i(zA(f))}}())]))(aa(function(i){return HO(ig.value)(function(){var f=pg(function(m){return function(){var D=mD(m)();return Av(D)("black")(),AD(D)({width:vg,height:_g,x:0,y:0})(),Av(D)("rgba(255,255,255,0.2)")(),bp(i)(function(A){return function(){return DD(D)(),og(D)({end:f9,radius:A.value1*40,start:0,x:A.value0.x*vg,y:A.value0.y*_g,useCounterClockwise:!1})(),bD(D)()}})()}});return function(m){return f(zA(m))}}())})(u.canvas))])([]),yi([lg(kv)([LX(di.value)("range"),RX(Wo.value)("0"),NX(Bo.value)("100"),UX(Ho.value)("1"),BX(zo.value)("50"),WX(Mt.value)("width: 100%;"),qX(qo.value)(Vr(function(){var i=pg(HX(Dc)(n.slider)),f=zX(yl);return function(m){return i(f(Zl(m)))}}()))])])([]),on([zO([kv(VX(Mt.value)("width:100%; padding:1.0rem;")),lg(aa(function(){var i=GX(ee.value);return function(f){return i(Vr(M(f)))}}()))([VO(u.startStop.loading)(fg(void 0)),GO(u.startStop.stop)(function(i){return dv(i)(dv(t(fg(void 0)))(n.startStop.start(void 0)))}),GO(JX(VO(a)(jX))(kD(kv(fg(void 0)))(aa(function(i){return i.value0})(e))))(function(i){return function(){i(),n.startStop.loading(void 0)();var m=Or(w.value)(),v=Ca(yD(XX)(function(D){return yD(QX(D))(function(A){return yD(KX(YX)(ZX(ut(D))(t9(y9))))(function(b){return yD(JO(y_(0)(5e4)))(function(_){var k=bf(qA(Sn(t_(b.pluck0))(nl(Kb(r9(b))))))({newSeed:Df(_),size:4});return JO(function(){var et=e9(function(H){return function(){var vt=Ui(),qr=Ui();return{x:vt,y:qr}}})(Ke(0)(127))(),Dt=Y(D)(o(D)(k)(m))(),vr=we(xf)(function(H){return function(){var vt=Ur(m)();return n9(vt)(function(qr){return function(){var fn=V_(qr)(),On=u9(function(){var ia=Lp(et),be=a9(function(ue){return function(Wu){return Wu/255}(ue)});return function(ue){return ia(be(ue))}}())(i9(fn))();return n.canvas(On)(),void 0}})()}})(),Gr=dv(dv(dv(Dt)(A))(o9(D)))(vr);return n.startStop.stop(Gr)(),Gr})})})})}))();return t(function(){return n.startStop.start(void 0)(),ii(Lo(v))()})(),void 0}})])])])([Ve(zO([aa(M("Turn off"))(u.startStop.stop),aa(M("Turn on"))(a),aa(M("Loading..."))(u.startStop.loading)]))])])}})})}}};var h9=mt({reflectType:function(){return`<div>
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
</div>`}})()()(W()(Je()(R)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"ex"}})({reflectSymbol:function(){return"ex"}})),S9=ou(Wt)(j),x9=rr(or),C9=function(){return d.value}(),ew=function(t){return function(r){return function(e){return function(n){var u=fu(t)(e);return h9(C9)({next:S9(n)(x9(r(hc.value))(Xe)),ex:rw(u)(r)(n)})}}}};var T9=mt({reflectType:function(){return`<div>
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
</div>`}})()()(Je()(R)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})),F9=F(j),E9=st(ke),M9=rr(or),O9=function(){return d.value}(),nw=function(t){return function(r){return function(e){return function(n){return T9(O9)({next:F9(E9(ee.value)(Vr(M(M9(r(Cm.value))(Xe)))))})}}}};var uw=W(),I9=mt({reflectType:function(){return`<section>
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
`}})()()(uw(uw(R)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}})),P9=wt(pt),L9=tr(cr),R9=rn(qt)(Dr),N9=X(),aw=F(j),iw=Uu(),U9=iw(En),B9=qu(hi),W9=B(ir),q9=iw(X0),H9=function(){return d.value}(),ow=function(t){return function(r){return function(e){return I9(H9)({txt:S(`\\ctx buf -> run2 ctx
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
  ]`),cancel:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([P9(1)([L9(u)(R9([N9,ga(1e3)(aw(U9({p:B9(W9(Ke(0)(60))([1,1.2,1,.8])),o:1.5,d:30}))),ga(3e3)(aw(q9({o:3.5})))]))])])}})})}}};var fw=W(),V9=mt({reflectType:function(){return`<section>
  <h2>Envelope</h2>
  <p>The <code>AudioEnvelope</code> parameter corresponds to the Web Audio API's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setValueCurveAtTime"><code>setValueCurveAtTime</code></a> function and sets an envelope <code>p</code> over the duration <code>d</code> starting at time <code>o</code>.</p>
  <pre><code>~txt~</code></pre>
  ~envelope~
  </section>
`}})()()(fw(fw(R)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}})),G9=wt(pt),J9=tr(cr),j9=rn(qt)(Dr),X9=X(),Q9=F(j),K9=Uu()(En),Y9=qu(hi),Z9=B(ir),tQ=function(){return d.value}(),cw=function(t){return function(r){return function(e){return V9(tQ)({txt:S(`\\ctx buf -> run2 ctx
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
  ]`),envelope:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([G9(1)([J9(u)(j9([X9,ga(1e3)(Q9(K9({p:Y9(Z9(Ke(0)(60))([1,1.2,1,.8])),o:1.5,d:30})))]))])])}})})}}};var eQ=Yt({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(W()(R)({reflectType:function(){return"numericEx"}})({reflectSymbol:function(){return"numericEx"}})),nQ=wt(pt),uQ=tr(cr),hD=aD(Wt),aQ=X(),SD=F(j),xD=Uu()(Ia),iQ=function(){return d.value}(),lw=function(t){return function(r){return function(e){return eQ(d.value)(iQ)({numericEx:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([nQ(1)([uQ(u)(hD(aQ)(function(){return hD(ga(1e3)(hD(SD(xD({n:1,o:1,t:bk})))(function(){return SD(xD({n:1.3,o:2,t:Ji}))})))(function(){return ga(2500)(hD(SD(xD({n:1,o:2.5,t:bk})))(function(){return SD(xD({n:.7,o:3.5,t:j0}))}))})}))])])}})})}}};var fQ=Yt({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(W()(R)({reflectType:function(){return"suddenEx"}})({reflectSymbol:function(){return"suddenEx"}})),cQ=wt(pt),lQ=tr(cr),pQ=rn(qt)(Dr),_Q=X(),vQ=F(j),sQ=Uu()(G0),mQ=function(){return d.value}(),pw=function(t){return function(r){return function(e){return fQ(d.value)(mQ)({suddenEx:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([cQ(1)([lQ(u)(pQ([_Q,ga(1500)(vQ(sQ({n:1.4})))]))])])}})})}}};var bQ=mt({reflectType:function(){return`<section>
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
`}})()()(W()(R)({reflectType:function(){return"unitEx"}})({reflectSymbol:function(){return"unitEx"}})),AQ=tr(cr),kQ=rn(qt)(Dr),sg=X(),dQ=F(j),yQ=Uu()(H0(lc)),_w=wt(pt),gQ=xm(sm),hQ=Rl(Am),SQ=R_(Tl),xQ=function(){return d.value}(),vw=function(t){return function(r){return function(e){return bQ(xQ)({unitEx:_t(e)(t)(function(n){return ut(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return Y(n)([AQ(u)(kQ([sg,dQ(yQ(q0(_w(1)([gQ(1)(sg),_w(.2)([hQ(100)([SQ(50)(sg)])])]))))]))])}})})}}};var $Q=rr(or),yv=W(),TQ=mt({reflectType:function(){return`<div>
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
</div>`}})()()(yv(yv(Je()(yv(yv(yv(R)({reflectType:function(){return"unit"}})({reflectSymbol:function(){return"unit"}}))({reflectType:function(){return"sudden"}})({reflectSymbol:function(){return"sudden"}}))({reflectType:function(){return"numeric"}})({reflectSymbol:function(){return"numeric"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}}))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}})),FQ=ou(Wt)(j),EQ=function(){return d.value}(),sw=function(t){return function(r){return function(e){return function(n){var u=$Q(r(H_.value))(Xe),a=fu(t)(e);return TQ(EQ)({sudden:pw(a)(r)(n),numeric:lw(a)(r)(n),envelope:cw(a)(r)(n),cancel:ow(a)(r)(n),unit:vw(a)(r)(n),next:FQ(n)(u)})}}}};var OQ=mt({reflectType:function(){return`<div>
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
</div>`}})()()(Je()(R)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})),wQ=F(j),IQ=st(ke),PQ=rr(or),LQ=function(){return d.value}(),mw=function(t){return function(r){return function(e){return function(n){return OQ(LQ)({next:wQ(IQ(ee.value)(Vr(M(PQ(r($m.value))(Xe)))))})}}}};var NQ=mt({reflectType:function(){return`<div>
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
</div>`}})()()(Je()(R)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})),UQ=F(j),BQ=st(ke),WQ=rr(or),qQ=function(){return d.value}(),Dw=function(t){return function(r){return function(e){return function(n){return NQ(qQ)({next:UQ(BQ(ee.value)(Vr(M(WQ(r(Sc.value))(Xe)))))})}}}};var zQ=Yt({reflectType:function(){return`<div>
  <h1>Imperative API</h1>

  <h2>Like JavaScript, but PureScript</h2>
  <p>
    If you're coming from the JavaScript or TypeScript world, or if you're a fan of monadic <code>do</code> notation, you may enjoy building things step-by-step rather than constructing large declarative structures. If you're that sort of person, this section is for you!
  </p>

  <h2>Parting shot</h2>
  <p>Thanks for checking out ocarina! We want it to be the most ergonomimc, expressive, and performant Web Audio API on your side of the Mississippi. It certainly is for me, and as I'm in Finland, I'm on <i>both sides</i> of the Mississippi, so you can't beat that! If you have any questions, comments, concerns or would just like to say "hi!", please check out the <a href="https://github.com/mikesol/purescript-ocarina">Ocarina GitHub Repo</a> or the <a href="https://purescript.org/chat">PureScript Discord's music channel</a>. Now go out there and play some ocarina!</p>
</div>`}})({reflectType:function(){return"~"}})()()(R),VQ=function(){return d.value}(),bw=function(t){return function(r){return function(e){return function(n){return zQ(d.value)(VQ)({})}}}};var Aw=W(),JQ=Yt({reflectType:function(){return`<section>
  <h2>Hello subgraph</h2>

  <p>Subgraphs have the type <code>Event (Event (Channel outputChannels payload))</code>. Streaming audio is a data type with two constructors: <code>sound</code> to create a subgraph and <code>silence</code> to turn it off. The inner event listens for sound/silence, and the outer event adds subgraphs to the scene. You can create as many subgraphs as you like: ocarina automatically frees up resources when you send the <code>silence</code> event. Note that, once you turn a subraph off with <code>silence</code>, you can't turn it back on again. In this case, just create a new subgraph.</p>

  <p>Here's a simple subgraph that is connected to a slider. As you slide the slider, new nodes are provisioned. Each one has a pseudo-random pitch.</p>

  <pre><code>@txt@</code></pre>
  @ex1@

</section>
`}})({reflectType:function(){return"@"}})()()(Aw(Aw(R)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})),kw=gt(Wt),gv=F(j),jQ=No(ft),XQ=da(me),QQ=wt(pt),hv=g(ft),dw=rn(qt)(Dr),KQ=au(w_(Zt(Cr()(kt(kt(xr)(aT)()()()({reflectSymbol:function(){return"playbackRate"}}))(O_)()()()({reflectSymbol:function(){return"buffer"}})))(Sr()()))),YQ=X(),yw=_u(qt)(Dr),ZQ=st(Zi),tK=st(Uf),rK=st(Nf),eK=st(Wf),nK=st(qf),uK=st(Bf),aK=st(ke),gw=B(ft),mg=F(Bt),hw=oe(ft),CD=rr(or),iK=at(pn),oK=Q(Z),Dg=ct(qn),fK=Mn(Ge),cK=Na(Ge),lK=de(Ge),pK=mn(ne),_K=`module Main where

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
`,vK=Bu(function(t){return Cn(function(r){return we(t)(function(e){return function(){var u=Ui();return r(e(u))()}})})}),sK=function(){return d.value}(),mK="https://freesound.org/data/previews/339/339810_5121236-lq.mp3",Sw=function(t){return function(r){return function(e){return JQ(d.value)(sK)({txt:S(_K),ex1:Ri(Dl)(function(n){return Ri(Dl)(function(u){return Ri(Dl)(function(a){return Ri(Dl)(function(o){var i=kw(gv(void 0))(n.value1),f=jQ(N.create)(vK)(XQ(function(v){return function(D){return v+1|0}})(0)(o.value1)),m=function(v){return[QQ(1)([Zp(hv(function(D){return dw([gv(B0(KQ({buffer:v,playbackRate:.7+pu(D)*2})(YQ))),ga(5e3)(gv(W0))])})(f))])]};return Rr([Rr([S("Slide me!"),yi([yw(gv)([ZQ(di.value)("range"),tK(Wo.value)("0"),rK(Bo.value)("100"),eK(Ho.value)("1"),nK(zo.value)("50"),uK(qo.value)(Vr(M(o.value0(void 0))))])])([])]),on([yw(hv(function(){var v=aK(ee.value);return function(D){return v(Vr(M(D)))}}()))([gw(a.value1)(mg(void 0)),hw(u.value1)(function(v){return CD(v)(CD(t(mg(void 0)))(n.value0(void 0)))}),hw(iK(gw(i)(oK))(kw(gv(mg(void 0)))(hv(function(v){return v.value0})(e))))(function(v){return function(){v(),a.value0(void 0)();var A=Ca(Dg(fK)(function(b){return Dg(cK(b))(function(_){return Dg(ut(b)(mK))(function(k){return lK(function(){var et=Im(m(k))(),Dt=CD(CD(et)(_))(pK(b));return u.value0(Dt)(),Dt})})})}))();return t(function(){return n.value0(void 0)(),ii(Lo(A))()})(),void 0}})])])([Ve(dw([hv(M("Turn off"))(u.value1),hv(M("Turn on"))(i)]))])])})})})})})}}};var xw=W(),bK=mt({reflectType:function(){return`<div>
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
</div>`}})()()(xw(xw(R)({reflectType:function(){return"suby"}})({reflectSymbol:function(){return"suby"}}))({reflectType:function(){return"appl"}})({reflectSymbol:function(){return"appl"}})),AK=wt(pt),kK=tr(cr),dK=X(),yK=function(){return d.value}(),Cw=function(t){return function(r){return function(e){return function(n){var u=fu(t)(e);return bK(yK)({appl:wm("\u{1F44F}")(n)(u)(function(a){return ut(a)("https://freesound.org/data/previews/277/277021_1402315-lq.mp3")})(function(a){return function(o){return Y(a)([AK(1)([kK(o)(dK)])])}}),suby:Sw(u)(r)(n)})}}}};var Wa=F(Ha),hK=da(me),SK=F(Bt),xK=g(ir),CK=gt(Wt),$K=_u(qt)(Dr),$w=F(j),Tw=st(ke),TK=st(zx),FK=g(ft),Fw=qp(ya),EK=st(As),Ew=Nt(Kf(io(uo))),OSr=function(t){return t},wSr={Coercible0:function(){}},MK=function(){var t=function(r){var e=function(n){if(n instanceof U_)return Rr(Wa(Be(ew(r.setCancellation)(r.setPage))));if(n instanceof hc)return Rr(Wa(Be(PO(r.setCancellation)(r.setPage))));if(n instanceof W_)return Rr(Wa(Be(MO(r.setCancellation)(r.setPage))));if(n instanceof B_)return Rr(Wa(Be(EE(r.setCancellation)(r.setPage))));if(n instanceof Cm)return Rr(Wa(Be(Dw(r.setCancellation)(r.setPage))));if(n instanceof Sc)return Rr(Wa(Be(kO(r.setCancellation)(r.setPage))));if(n instanceof q_)return Rr(Wa(Be(sw(r.setCancellation)(r.setPage))));if(n instanceof H_)return Rr(Wa(Be(WM(r.setCancellation)(r.setPage))));if(n instanceof $m)return Rr(Wa(Be(bw(r.setCancellation)(r.setPage))));if(n instanceof cF)return Rr(Wa(Be(nw(r.setCancellation)(r.setPage))));if(n instanceof z_)return Rr(Wa(Be(Cw(r.setCancellation)(r.setPage))));if(n instanceof lF)return Rr(Wa(Be(mw(r.setCancellation)(r.setPage))));throw new Error("Failed pattern match at Ocarina.Example.Docs (line 140, column 5 - line 140, column 76): "+[n.constructor.name])};return e(r.page)};return Ri(hC(new Nl(U_.value)))(function(r){var e=hK(Gt(function(n){if(n instanceof Nl)return function(u){return{prevPage:new $(u.curPage),curPage:n.value0,cancel:u.cancel,pageChange:!0}};if(n instanceof Wk)return function(u){return{cancel:n.value0,pageChange:!1,curPage:u.curPage,prevPage:u.prevPage}};throw new Error("Failed pattern match at Ocarina.Example.Docs (line 51, column 13 - line 53, column 81): "+[n.constructor.name])}))({prevPage:w.value,curPage:U_.value,cancel:SK(void 0),pageChange:!0})(r.value1);return Rr([Rr(xK(function(n){return cA([fA([CK($K($w)([Tw(ee.value)(Vr(M(r.value0(new Nl(n.value0))))),TK(Mt.value)("cursor:pointer;")]))(FK(function(u){return Tw(ee.value)(Vr(M(function(){return u.cancel(),r.value0(new Nl(n.value0))()})))})(Fw(function(u){return!function(a){return a.pageChange}(u)})(e)))])([S(n.value1.value0)]),cl([$w(EK(Mt.value)(function(){return n.value1.value1?"":"display:none;"}()))])([S(" | ")])])})([new N(U_.value,new N("Home",!0)),new N(hc.value,new N("Hello world",!0)),new N(W_.value,new N("Array, fan, and fix",!0)),new N(B_.value,new N("Audio units",!0)),new N(Sc.value,new N("Events",!0)),new N(q_.value,new N("Parameters",!0)),new N(H_.value,new N("State",!0)),new N(z_.value,new N("Subgraphs",!1))])),Nx(function(n){return t({page:n.curPage,setPage:function(u){return r.value0(Nl.create(u))},setCancellation:function(u){return r.value0(Wk.create(u))}})})(Fw(function(n){return n.pageChange})(e))])})}(),ISr=function(t){return{page:t,setPage:Ew,setCancellation:Ew}},PSr=J$(MK);export{OSr as TopLevelSg,PSr as main,wSr as newtypeTopLevelSg_,ISr as p2tl,MK as scene};
