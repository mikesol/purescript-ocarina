function Rb(t){return()=>t.slice()}function Nb(t){return e=>r=>()=>{r[t]=e}}function Lb(t){return()=>t.slice()}var Wb=function(t){return function(e){return function(r){return function(n){return function(a){return n<a?t:n===a?e:r}}}}};var Bb=Wb,Gb=Wb;var Ub=function(t){return function(e){return t===e}};var qb=Ub,Hb=Ub;var d=function(){function t(){}return t.value=new t,t}();var Sr=function(t){return t.reflectSymbol};var z_=function(t){return function(e){return{}.hasOwnProperty.call(e,t)}},qa=function(t){return function(e){return e[t]}},Gu=function(t){return function(e){return function(r){var n={};for(var a in r)({}).hasOwnProperty.call(r,a)&&(n[a]=r[a]);return n[t]=e,n}}};var nl={eq:Hb},wi={eq:qb};var Yt=function(t){return t.eq};var Zt=function(){function t(){}return t.value=new t,t}(),de=function(){function t(){}return t.value=new t,t}(),be=function(){function t(){}return t.value=new t,t}();var zb=function(t){return function(e){return t-e|0}},Vb=function(t){return function(e){return t-e}};var Jb=function(t){return function(e){return t+e|0}},jb=function(t){return function(e){return t*e|0}},Xb=function(t){return function(e){return t+e}},Qb=function(t){return function(e){return t*e}};var va=function(t){return t.zero};var Da={add:Xb,zero:0,mul:Qb,one:1},Su={add:Jb,zero:0,mul:jb,one:1};var da=function(t){return t.one};var $n=function(t){return t.mul};var Le=function(t){return t.add};var hu=function(t){return t.sub};var lf={sub:Vb,Semiring0:function(){return Da}},em={sub:zb,Semiring0:function(){return Su}};var al=function(t){return function(e){return hu(t)(va(t.Semiring0()))(e)}};var $a=function(){return{compare:Gb(Zt.value)(be.value)(de.value),Eq0:function(){return nl}}}(),qr=function(){return{compare:Bb(Zt.value)(be.value)(de.value),Eq0:function(){return wi}}}();var te=function(t){return t.compare};var Yb=function(t){return function(e){return function(r){var n=te(t)(e)(r);return!(n instanceof Zt)}}};var Tu=function(t){return function(e){return function(r){var n=te(t)(e)(r);if(n instanceof Zt)return r;if(n instanceof be||n instanceof de)return e;throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): "+[n.constructor.name])}}};var nm=function(t){return function(e){return function(r){var n=Yb(t)(r)(va(e.Semiring0()));return n?r:al(e)(r)}}};var Zb=function(t){return function(e){for(var r=t.length,n=e.length,a=new Array(r*n),u=0,f=0;f<r;f++)for(var i=t[f],m=0;m<n;m++)a[u++]=i(e[m]);return a}};var Qo={compose:function(t){return function(e){return function(r){return t(e(r))}}}},yo=function(t){return t.compose};var Z=function(t){return t.identity},tt={identity:function(t){return t},Semigroupoid0:function(){return Qo}};var tr=!0;var Tt=function(t){return function(e){return function(r){return t(r)(e)}}},T=function(t){return function(e){return t}};var Vf=function(t){return function(e){return e(t)}},ol=function(t){return function(e){return t(e)}};var tA=function(t){return function(e){for(var r=e.length,n=new Array(r),a=0;a<r;a++)n[a]=t(e[a]);return n}};var p=function(t){return t.map},Ye=function(t){return function(e){return function(r){return p(t)(r)(e)}}},Ae=function(t){return p(t)(T(void 0))},X=function(t){return function(e){return function(r){return p(t)(T(r))(e)}}},J_=function(t){return function(e){return p(t)(T(e))}};var Ma={map:yo(Qo)},xe={map:tA},am=function(t){return function(e){return function(r){return p(t)(function(n){return n(r)})(e)}}};var il={apply:Zb,Functor0:function(){return xe}},Gt=function(t){return t.apply};var j=function(t){return function(e){return function(r){return Gt(t)(p(t.Functor0())(T(Z(tt)))(e))(r)}}},ea=function(t){return function(e){return function(r){return function(n){return Gt(t)(p(t.Functor0())(e)(r))(n)}}}};var h=function(t){return t.pure};var Mn=function(t){return function(e){return function(r){if(e)return r;if(!e)return h(t)(void 0);throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): "+[e.constructor.name,r.constructor.name])}}},fl=function(t){return function(e){return function(r){return Gt(t.Apply0())(h(t)(e))(r)}}};var fr={pure:function(t){return[t]},Apply0:function(){return il}};var eA=function(t){return function(e){for(var r=[],n=0,a=t.length;n<a;n++)Array.prototype.push.apply(r,e(t[n]));return r}};var er=function(t){return t.discard};var Ko={bind:eA,Apply0:function(){return il}},U=function(t){return t.bind},qn=function(t){return Tt(U(t))};var Jf=function(t){return function(e){return function(r){return function(n){return U(t)(e(n))(r)}}}};var Xe={discard:function(t){return U(t)}};var Ha=function(t){return function(e){return U(t)(e)(Z(tt))}};var ct=function(t){return t};var rA=ct;var nA=function(t){return function(e){return function(){return t(e())}}};function ra(t){return function(){return{value:t}}}var wn=function(t){return function(){return t.value}},aA=function(t){return function(e){return function(){var r=t(e.value);return e.value=r.state,r.value}}},wa=function(t){return function(e){return function(){return e.value=t}}};var Uu=function(t){return function(e){return function(r){return U(t.Bind1())(e)(function(n){return U(t.Bind1())(r)(function(a){return h(t.Applicative0())(n(a))})})}}};var uA=function(t){return function(e){return t.length===0?e:e.length===0?t:t.concat(e)}};var pf=function(t){var e=function(r){var n;function a(u){r=u}for(;;)n=a(r);return n};return e(t)};var oA={append:function(t){return function(e){return void 0}}};var ba={append:uA};var gt=function(t){return t.append},im=function(t){return{append:function(e){return function(r){return function(n){return gt(t)(e(n))(r(n))}}}}};var I=function(t){return t.alt};var wh=String.fromCharCode(65535),Ph=String.fromCharCode(0),Oh=Number.POSITIVE_INFINITY,Ih=Number.NEGATIVE_INFINITY;var Hn=function(t){return t.top};var sf={top:2147483647,bottom:-2147483648,Ord0:function(){return qr}};var zn=function(t){return t.bottom};var fA=function(t){return t.toString()},cA=function(t){var e=t.toString();return isNaN(e+".0")?e:e+".0"};var X_={show:cA},za={show:fA};var zt=function(t){return t.show};var z=function(){function t(){}return t.value=new t,t}(),N=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var Kt=function(t){return function(e){return function(r){if(r instanceof z)return t;if(r instanceof N)return e(r.value0);throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}};var $r={map:function(t){return function(e){return e instanceof N?new N(t(e.value0)):z.value}}};var Aa=function(t){return Kt(t)(Z(tt))},Jn=function(){return function(t){if(t instanceof N)return t.value0;throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): "+[t.constructor.name])}};var Oi={apply:function(t){return function(e){if(t instanceof N)return p($r)(t.value0)(e);if(t instanceof z)return z.value;throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): "+[t.constructor.name,e.constructor.name])}},Functor0:function(){return $r}},ya={bind:function(t){return function(e){if(t instanceof N)return e(t.value0);if(t instanceof z)return z.value;throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): "+[t.constructor.name,e.constructor.name])}},Apply0:function(){return Oi}};var go=function(){return{pure:N.create,Apply0:function(){return Oi}}}();var Jt=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),jt=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var vf={map:function(t){return function(e){if(e instanceof Jt)return new Jt(e.value0);if(e instanceof jt)return new jt(t(e.value0));throw new Error("Failed pattern match at Data.Either (line 31, column 1 - line 31, column 52): "+[e.constructor.name])}}};var Pa=function(t){return function(e){return function(r){if(r instanceof Jt)return t(r.value0);if(r instanceof jt)return e(r.value0);throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}},Q_=function(){return Pa(T(z.value))(N.create)}();var au=function(t){return t};var Co={map:function(t){return function(e){return t(e)}}};var lA={apply:function(t){return function(e){return t(e)}},Functor0:function(){return Co}},Vh={bind:function(t){return function(e){return e(t)}},Apply0:function(){return lA}},cm={pure:au,Apply0:function(){return lA}},Hu={Applicative0:function(){return cm},Bind1:function(){return Vh}};var _A=function(t){return Math.min(Math.abs(t),2147483647)},pA=function(t){return function(e){return e===0?0:e>0?Math.floor(t/e):-Math.floor(t/-e)}},sA=function(t){return function(e){if(e===0)return 0;var r=Math.abs(e);return(t%r+r)%r}},mA=function(t){return function(e){return t/e}};var vA={Ring0:function(){return lf}},DA={Ring0:function(){return em}};var uu=function(t){return t.mod};var pl={degree:function(t){return 1},div:mA,mod:function(t){return function(e){return 0}},CommutativeRing0:function(){return vA}},Eo={degree:_A,div:pA,mod:sA,CommutativeRing0:function(){return DA}},zu=function(t){return t.div};var Zr={mempty:void 0,Semigroup0:function(){return oA}};var Ot=function(t){return t.mempty},Hr=function(t){return{mempty:function(e){return Ot(t)},Semigroup0:function(){return im(t.Semigroup0())}}};var lm=function(t){return function(){return t}},dA=function(t){return function(e){return function(){return e(t())()}}};var sl=function(t){return function(e){return function(){for(var r=0,n=t.length;r<n;r++)e(t[r])()}}};var bA=function(t,e,r){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return n=1,a=r(),n=2,a}},ae={Applicative0:function(){return c},Bind1:function(){return Cn}},Cn={bind:dA,Apply0:function(){return _m(0)}},c={pure:lm,Apply0:function(){return _m(0)}},AA=bA("functorEffect","Effect",function(){return{map:fl(c)}}),_m=bA("applyEffect","Effect",function(){return{apply:Uu(ae),Functor0:function(){return AA(0)}}}),x=AA(20),rt=_m(23),yA=function(t){return{append:ea(rt)(gt(t))}},zr=function(t){return{mempty:lm(Ot(t)),Semigroup0:function(){return yA(t.Semigroup0())}}};var kA=function(t){return function(){return{value:t}}};var Mr=function(t){return function(){return t.value}},gA=function(t){return function(e){return function(){var r=t(e.value);return e.value=r.state,r.value}}},En=function(t){return function(e){return function(){e.value=t}}};var cr=kA,Yh=gA,Xf=function(t){return Yh(function(e){var r=t(e);return{state:r,value:r}})},Df=function(t){return function(e){return Ae(x)(Xf(t)(e))}};var nT=aA,Xu=function(t){return nT(function(e){var r=t(e);return{state:r,value:r}})},Ii={map:nA};var o={liftST:rA,Monad0:function(){return ae}},Fe=function(t){return t.liftST};var ln=function(t){return function(e){for(var r=t>e?-1:1,n=new Array(r*(e-t)+1),a=t,u=0;a!==e;)n[u++]=a,a+=r;return n[u]=a,n}},uT=function(t){return function(e){if(t<1)return[];var r=new Array(t);return r.fill(e)}},oT=function(t){return function(e){for(var r=[],n=0,a=0;a<t;a++)r[n++]=e;return r}},K_=typeof Array.prototype.fill=="function"?uT:oT,iT=function(){function t(a,u){this.head=a,this.tail=u}var e={};function r(a){return function(u){return new t(a,u)}}function n(a){for(var u=[],f=0,i=a;i!==e;)u[f++]=i.head,i=i.tail;return u}return function(a){return function(u){return n(a(r)(e)(u))}}}(),xu=function(t){return t.length};var EA=function(t){return function(e){return function(r){return function(n){for(var a=0,u=n.length;a<u;a++)if(r(n[a]))return t(a);return e}}}};var SA=function(t){return function(e){return function(r){return function(n){if(r<0||r>=n.length)return e;var a=n.slice();return a.splice(r,1),t(a)}}}};var fT=function(){function t(e,r,n,a,u,f){var i,m,s,_,v,D,l;for(i=u+(f-u>>1),i-u>1&&t(e,r,a,n,u,i),f-i>1&&t(e,r,a,n,i,f),m=u,s=i,_=u;m<i&&s<f;)v=a[m],D=a[s],l=r(e(v)(D)),l>0?(n[_++]=D,++s):(n[_++]=v,++m);for(;m<i;)n[_++]=a[m++];for(;s<f;)n[_++]=a[s++]}return function(e){return function(r){return function(n){var a;return n.length<2?n:(a=n.slice(0),t(e,r,a,n.slice(0),0,n.length),a)}}}}();var ml=function(t){return function(e){return function(r){for(var n=e.length<r.length?e.length:r.length,a=new Array(n),u=0;u<n;u++)a[u]=t(e[u])(r[u]);return a}}};var hA=function(t){return function(e){return t[e]}};var lT=function(t,e,r){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return n=1,a=r(),n=2,a}};var TA={defer:function(t){return function(e){return t(void 0)(e)}}},Qf=function(t){return t.defer},sm=function(t){return function(e){var r=lT("go","Control.Lazy",function(){return Qf(t)(function(a){return e(r(25))})}),n=r(25);return n}};var _T=function(){function t(e,r,n,a,u,f){var i,m,s,_,v,D,l;for(i=u+(f-u>>1),i-u>1&&t(e,r,a,n,u,i),f-i>1&&t(e,r,a,n,i,f),m=u,s=i,_=u;m<i&&s<f;)v=a[m],D=a[s],l=r(e(v)(D)),l>0?(n[_++]=D,++s):(n[_++]=v,++m);for(;m<i;)n[_++]=a[m++];for(;s<f;)n[_++]=a[s++]}return function(e){return function(r){return function(n){return function(){return n.length<2||t(e,r,n,n.slice(0),0,n.length),n}}}}}();var wA=function(t){return function(e){return t&&e}},PA=function(t){return function(e){return t||e}},OA=function(t){return!t};var fu=function(t){return t.not};var Yf=function(t){return t.disj},Ia={ff:!1,tt:!0,implies:function(t){return function(e){return Yf(Ia)(fu(Ia)(t))(e)}},conj:wA,disj:PA,not:OA};var RA=function(t){return function(e){return function(r){for(var n=e,a=r.length,u=a-1;u>=0;u--)n=t(r[u])(n);return n}}},NA=function(t){return function(e){return function(r){for(var n=e,a=r.length,u=0;u<a;u++)n=t(n)(r[u]);return n}}};var M=function(t){return t.empty};var ut=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}(),Zf=function(t){return function(e){return t(e.value0)(e.value1)}};var tn=function(t){return t.value1};var Yo={map:function(t){return function(e){return new ut(e.value0,t(e.value1))}}};var Na=function(t){return t.value0};var Fu=function(){return ct};var en=Fu,Ar=Fu;var km=function(){return function(){return function(t){return Fu()}}};var rr=function(t){return t.foldr};var kr=function(t){return function(e){return rr(t)(I(e.Alt0()))(M(e))}},_n=function(t){return function(e){return function(r){return rr(t)(function(){var n=I(e.Alt0());return function(a){return n(r(a))}}())(M(e))}}},pr=function(t){return function(e){return function(r){return rr(e)(function(){var n=j(t.Apply0());return function(a){return n(r(a))}}())(h(t)(void 0))}}},na=function(t){return function(e){return Tt(pr(t)(e))}},ap=function(t){return function(e){return pr(t)(e)(Z(tt))}},dr=function(t){return t.foldl};var Ve={foldr:function(t){return function(e){return function(r){if(r instanceof z)return e;if(r instanceof N)return t(r.value0)(e);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}},foldl:function(t){return function(e){return function(r){if(r instanceof z)return e;if(r instanceof N)return t(e)(r.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}},foldMap:function(t){return function(e){return function(r){if(r instanceof z)return Ot(t);if(r instanceof N)return e(r.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[e.constructor.name,r.constructor.name])}}}};var UA=function(t){return function(e){return function(r){return rr(t)(function(n){return function(a){return gt(e.Semigroup0())(r(n))(a)}})(Ot(e))}}},Xt={foldr:RA,foldl:NA,foldMap:function(t){return UA(Xt)(t)}};var Pn=function(t){return t.foldMap};var qA=function(){function t(a){return[a]}function e(a){return function(u){return[a,u]}}function r(a){return function(u){return function(f){return[a,u,f]}}}function n(a){return function(u){return a.concat(u)}}return function(a){return function(u){return function(f){return function(i){return function(m){function s(_,v){switch(v-_){case 0:return f([]);case 1:return u(t)(i(m[_]));case 2:return a(u(e)(i(m[_])))(i(m[_+1]));case 3:return a(a(u(r)(i(m[_])))(i(m[_+1])))(i(m[_+2]));default:var D=_+Math.floor((v-_)/4)*2;return a(u(n)(s(_,D)))(s(D,v))}}return s(0,m.length)}}}}}}();var jn=function(t){return t.traverse};var sx=function(t){return function(e){return jn(t)(e)(Z(tt))}},Zo={traverse:function(t){return qA(Gt(t.Apply0()))(p(t.Apply0().Functor0()))(h(t))},sequence:function(t){return sx(Zo)(t)},Functor0:function(){return xe},Foldable1:function(){return Xt}};var El=function(){return ml(ut.create)}();var Om=function(){return hA};var ay=function(t){return[t]};var uy=function(){return EA(N.create)(z.value)}();var Im=function(){return SA(N.create)(z.value)}(),Rm=function(t){return function(e){return function(r){return r.length===0?[]:Kt(r)(function(n){return Jn()(Im(n)(r))})(uy(t(e))(r))}}};var Li=function(t){return function(e){return gt(ba)([t])(e)}};var oy=function(t){return function(e){for(var r=e.length,n=Array(r),a=0;a<r;a++)n[a]=t(a)(e[a]);return n}};var To=function(t){return t.mapWithIndex};var Wi={mapWithIndex:oy,Functor0:function(){return xe}};var Fo=function(t){return t.foldrWithIndex};var Qu=function(t){return t.foldlWithIndex};var ei=function(t){return t.foldMapWithIndex};var Bi=function(t){return t.traverseWithIndex};var Ku=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}();var sp=function(t){return function(e){return new Ku(e,M(t))}};var wr=function(){function t(){}return t.value=new t,t}(),le=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}(),mp=function(t){return t},qx=function(t){return new le(t.value0,t.value1)};var Hx=function(t){var e=function(r){return function(n){var a=r,u=!1,f;function i(m,s){if(s instanceof le&&s.value1 instanceof le&&s.value1.value1 instanceof le){a=new le(s,m),n=s.value1.value1.value1;return}var _=function(D){return D instanceof le&&D.value1 instanceof le&&D.value1.value1 instanceof wr?new le(t(D.value0),new le(t(D.value1.value0),wr.value)):D instanceof le&&D.value1 instanceof wr?new le(t(D.value0),wr.value):wr.value},v=function(D){return function(l){var g=D,nt=!1,st;function me(Qt,Je){if(Qt instanceof le&&Qt.value0 instanceof le&&Qt.value0.value1 instanceof le&&Qt.value0.value1.value1 instanceof le){g=Qt.value1,l=new le(t(Qt.value0.value0),new le(t(Qt.value0.value1.value0),new le(t(Qt.value0.value1.value1.value0),Je)));return}return nt=!0,Je}for(;!nt;)st=me(g,l);return st}};return u=!0,v(m)(_(s))}for(;!u;)f=i(a,n);return f}};return e(wr.value)},vp={map:Hx};var La={foldr:function(t){return function(e){var r=function(){var a=function(u){return function(f){var i=u,m=!1,s;function _(v,D){if(D instanceof wr)return m=!0,v;if(D instanceof le){i=new le(D.value0,v),f=D.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): "+[v.constructor.name,D.constructor.name])}for(;!m;)s=_(i,f);return s}};return a(wr.value)}(),n=dr(La)(Tt(t))(e);return function(a){return n(r(a))}}},foldl:function(t){var e=function(r){return function(n){var a=r,u=!1,f;function i(m,s){if(s instanceof wr)return u=!0,m;if(s instanceof le){a=t(m)(s.value0),n=s.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): "+[s.constructor.name])}for(;!u;)f=i(a,n);return f}};return e},foldMap:function(t){return function(e){return dr(La)(function(r){var n=gt(t.Semigroup0())(r);return function(a){return n(e(a))}})(Ot(t))}}};var Sl={append:function(t){return function(e){return rr(La)(le.create)(e)(t)}}};var Lm={append:function(t){return function(e){return new Ku(t.value0,gt(Sl)(t.value1)(qx(e)))}}};var ly={alt:gt(Sl),Functor0:function(){return vp}},Wm=function(){return{empty:wr.value,Alt0:function(){return ly}}}();var Dy=function(t){return t()};var dy=function(t){throw new Error(t)};var by=function(){return dy};var _F=Dy,lu=function(t){return _F(function(){return by()(t)})};var Vt=function(){function t(){}return t.value=new t,t}(),pe=function(){function t(e,r,n,a){this.value0=e,this.value1=r,this.value2=n,this.value3=a}return t.create=function(e){return function(r){return function(n){return function(a){return new t(e,r,n,a)}}}},t}(),We=function(){function t(e,r,n,a,u,f,i){this.value0=e,this.value1=r,this.value2=n,this.value3=a,this.value4=u,this.value5=f,this.value6=i}return t.create=function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return function(i){return new t(e,r,n,a,u,f,i)}}}}}}},t}(),Gi=function(){function t(e,r,n){this.value0=e,this.value1=r,this.value2=n}return t.create=function(e){return function(r){return function(n){return new t(e,r,n)}}},t}(),oi=function(){function t(e,r,n){this.value0=e,this.value1=r,this.value2=n}return t.create=function(e){return function(r){return function(n){return new t(e,r,n)}}},t}(),Ui=function(){function t(e,r,n,a,u,f){this.value0=e,this.value1=r,this.value2=n,this.value3=a,this.value4=u,this.value5=f}return t.create=function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return new t(e,r,n,a,u,f)}}}}}},t}(),$o=function(){function t(e,r,n,a,u,f){this.value0=e,this.value1=r,this.value2=n,this.value3=a,this.value4=u,this.value5=f}return t.create=function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return new t(e,r,n,a,u,f)}}}}}},t}(),qi=function(){function t(e,r,n,a,u,f){this.value0=e,this.value1=r,this.value2=n,this.value3=a,this.value4=u,this.value5=f}return t.create=function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return new t(e,r,n,a,u,f)}}}}}},t}(),dp=function(){function t(e,r,n,a){this.value0=e,this.value1=r,this.value2=n,this.value3=a}return t.create=function(e){return function(r){return function(n){return function(a){return new t(e,r,n,a)}}}},t}();var yy=function(t){return function(e){return new pe(Vt.value,t,e,Vt.value)}};var dF=function(t){return function(e){var r=te(t),n=function(a){var u=!1,f;function i(m){if(m instanceof Vt)return u=!0,z.value;if(m instanceof pe){var s=r(e)(m.value1);if(s instanceof be)return u=!0,new N(m.value2);if(s instanceof Zt){a=m.value0;return}a=m.value3;return}if(m instanceof We){var _=r(e)(m.value1);if(_ instanceof be)return u=!0,new N(m.value2);var v=r(e)(m.value4);if(v instanceof be)return u=!0,new N(m.value5);if(_ instanceof Zt){a=m.value0;return}if(v instanceof de){a=m.value6;return}a=m.value3;return}throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): "+[m.constructor.name])}for(;!u;)f=i(a);return f};return n}};var ky=function(t){return t instanceof Vt};var rn=function(t){return function(e){return function(r){var n=t,a=e,u=!1,f;function i(m,s,_){if(s instanceof wr)return u=!0,_;if(s instanceof le){if(s.value0 instanceof Gi){n=m,a=s.value1,r=new pe(_,s.value0.value0,s.value0.value1,s.value0.value2);return}if(s.value0 instanceof oi){n=m,a=s.value1,r=new pe(s.value0.value0,s.value0.value1,s.value0.value2,_);return}if(s.value0 instanceof Ui){n=m,a=s.value1,r=new We(_,s.value0.value0,s.value0.value1,s.value0.value2,s.value0.value3,s.value0.value4,s.value0.value5);return}if(s.value0 instanceof $o){n=m,a=s.value1,r=new We(s.value0.value0,s.value0.value1,s.value0.value2,_,s.value0.value3,s.value0.value4,s.value0.value5);return}if(s.value0 instanceof qi){n=m,a=s.value1,r=new We(s.value0.value0,s.value0.value1,s.value0.value2,s.value0.value3,s.value0.value4,s.value0.value5,_);return}throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): "+[s.value0.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): "+[s.constructor.name,_.constructor.name])}for(;!u;)f=i(n,a,r);return f}}},xl=function(t){return function(e){return function(r){var n=function(f){return function(i){var m=f,s=!1,_;function v(D,l){if(D instanceof wr)return s=!0,new pe(l.value0,l.value1,l.value2,l.value3);if(D instanceof le){if(D.value0 instanceof Gi)return s=!0,rn(t)(D.value1)(new We(l.value0,l.value1,l.value2,l.value3,D.value0.value0,D.value0.value1,D.value0.value2));if(D.value0 instanceof oi)return s=!0,rn(t)(D.value1)(new We(D.value0.value0,D.value0.value1,D.value0.value2,l.value0,l.value1,l.value2,l.value3));if(D.value0 instanceof Ui){m=D.value1,i=new dp(new pe(l.value0,l.value1,l.value2,l.value3),D.value0.value0,D.value0.value1,new pe(D.value0.value2,D.value0.value3,D.value0.value4,D.value0.value5));return}if(D.value0 instanceof $o){m=D.value1,i=new dp(new pe(D.value0.value0,D.value0.value1,D.value0.value2,l.value0),l.value1,l.value2,new pe(l.value3,D.value0.value3,D.value0.value4,D.value0.value5));return}if(D.value0 instanceof qi){m=D.value1,i=new dp(new pe(D.value0.value0,D.value0.value1,D.value0.value2,D.value0.value3),D.value0.value4,D.value0.value5,new pe(l.value0,l.value1,l.value2,l.value3));return}throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): "+[D.value0.constructor.name,l.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): "+[D.constructor.name,l.constructor.name])}for(;!s;)_=v(m,i);return _}},a=te(t),u=function(f){return function(i){var m=f,s=!1,_;function v(D,l){if(l instanceof Vt)return s=!0,n(D)(new dp(Vt.value,e,r,Vt.value));if(l instanceof pe){var g=a(e)(l.value1);if(g instanceof be)return s=!0,rn(t)(D)(new pe(l.value0,e,r,l.value3));if(g instanceof Zt){m=new le(new Gi(l.value1,l.value2,l.value3),D),i=l.value0;return}m=new le(new oi(l.value0,l.value1,l.value2),D),i=l.value3;return}if(l instanceof We){var nt=a(e)(l.value1);if(nt instanceof be)return s=!0,rn(t)(D)(new We(l.value0,e,r,l.value3,l.value4,l.value5,l.value6));var st=a(e)(l.value4);if(st instanceof be)return s=!0,rn(t)(D)(new We(l.value0,l.value1,l.value2,l.value3,e,r,l.value6));if(nt instanceof Zt){m=new le(new Ui(l.value1,l.value2,l.value3,l.value4,l.value5,l.value6),D),i=l.value0;return}if(nt instanceof de&&st instanceof Zt){m=new le(new $o(l.value0,l.value1,l.value2,l.value4,l.value5,l.value6),D),i=l.value3;return}m=new le(new qi(l.value0,l.value1,l.value2,l.value3,l.value4,l.value5),D),i=l.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): "+[D.constructor.name,l.constructor.name])}for(;!s;)_=v(m,i);return _}};return u(wr.value)}}},bF=function(t){return function(e){var r=function(i){return function(m){var s=i,_=!1,v;function D(l,g){if(l instanceof wr)return _=!0,g;if(l instanceof le){if(l.value0 instanceof Gi&&l.value0.value2 instanceof Vt&&g instanceof Vt)return _=!0,rn(t)(l.value1)(new pe(Vt.value,l.value0.value0,l.value0.value1,Vt.value));if(l.value0 instanceof oi&&l.value0.value0 instanceof Vt&&g instanceof Vt)return _=!0,rn(t)(l.value1)(new pe(Vt.value,l.value0.value1,l.value0.value2,Vt.value));if(l.value0 instanceof Gi&&l.value0.value2 instanceof pe){s=l.value1,m=new We(g,l.value0.value0,l.value0.value1,l.value0.value2.value0,l.value0.value2.value1,l.value0.value2.value2,l.value0.value2.value3);return}if(l.value0 instanceof oi&&l.value0.value0 instanceof pe){s=l.value1,m=new We(l.value0.value0.value0,l.value0.value0.value1,l.value0.value0.value2,l.value0.value0.value3,l.value0.value1,l.value0.value2,g);return}return l.value0 instanceof Gi&&l.value0.value2 instanceof We?(_=!0,rn(t)(l.value1)(new pe(new pe(g,l.value0.value0,l.value0.value1,l.value0.value2.value0),l.value0.value2.value1,l.value0.value2.value2,new pe(l.value0.value2.value3,l.value0.value2.value4,l.value0.value2.value5,l.value0.value2.value6)))):l.value0 instanceof oi&&l.value0.value0 instanceof We?(_=!0,rn(t)(l.value1)(new pe(new pe(l.value0.value0.value0,l.value0.value0.value1,l.value0.value0.value2,l.value0.value0.value3),l.value0.value0.value4,l.value0.value0.value5,new pe(l.value0.value0.value6,l.value0.value1,l.value0.value2,g)))):l.value0 instanceof Ui&&l.value0.value2 instanceof Vt&&l.value0.value5 instanceof Vt&&g instanceof Vt?(_=!0,rn(t)(l.value1)(new We(Vt.value,l.value0.value0,l.value0.value1,Vt.value,l.value0.value3,l.value0.value4,Vt.value))):l.value0 instanceof $o&&l.value0.value0 instanceof Vt&&l.value0.value5 instanceof Vt&&g instanceof Vt?(_=!0,rn(t)(l.value1)(new We(Vt.value,l.value0.value1,l.value0.value2,Vt.value,l.value0.value3,l.value0.value4,Vt.value))):l.value0 instanceof qi&&l.value0.value0 instanceof Vt&&l.value0.value3 instanceof Vt&&g instanceof Vt?(_=!0,rn(t)(l.value1)(new We(Vt.value,l.value0.value1,l.value0.value2,Vt.value,l.value0.value4,l.value0.value5,Vt.value))):l.value0 instanceof Ui&&l.value0.value2 instanceof pe?(_=!0,rn(t)(l.value1)(new pe(new We(g,l.value0.value0,l.value0.value1,l.value0.value2.value0,l.value0.value2.value1,l.value0.value2.value2,l.value0.value2.value3),l.value0.value3,l.value0.value4,l.value0.value5))):l.value0 instanceof $o&&l.value0.value0 instanceof pe?(_=!0,rn(t)(l.value1)(new pe(new We(l.value0.value0.value0,l.value0.value0.value1,l.value0.value0.value2,l.value0.value0.value3,l.value0.value1,l.value0.value2,g),l.value0.value3,l.value0.value4,l.value0.value5))):l.value0 instanceof $o&&l.value0.value5 instanceof pe?(_=!0,rn(t)(l.value1)(new pe(l.value0.value0,l.value0.value1,l.value0.value2,new We(g,l.value0.value3,l.value0.value4,l.value0.value5.value0,l.value0.value5.value1,l.value0.value5.value2,l.value0.value5.value3)))):l.value0 instanceof qi&&l.value0.value3 instanceof pe?(_=!0,rn(t)(l.value1)(new pe(l.value0.value0,l.value0.value1,l.value0.value2,new We(l.value0.value3.value0,l.value0.value3.value1,l.value0.value3.value2,l.value0.value3.value3,l.value0.value4,l.value0.value5,g)))):l.value0 instanceof Ui&&l.value0.value2 instanceof We?(_=!0,rn(t)(l.value1)(new We(new pe(g,l.value0.value0,l.value0.value1,l.value0.value2.value0),l.value0.value2.value1,l.value0.value2.value2,new pe(l.value0.value2.value3,l.value0.value2.value4,l.value0.value2.value5,l.value0.value2.value6),l.value0.value3,l.value0.value4,l.value0.value5))):l.value0 instanceof $o&&l.value0.value0 instanceof We?(_=!0,rn(t)(l.value1)(new We(new pe(l.value0.value0.value0,l.value0.value0.value1,l.value0.value0.value2,l.value0.value0.value3),l.value0.value0.value4,l.value0.value0.value5,new pe(l.value0.value0.value6,l.value0.value1,l.value0.value2,g),l.value0.value3,l.value0.value4,l.value0.value5))):l.value0 instanceof $o&&l.value0.value5 instanceof We?(_=!0,rn(t)(l.value1)(new We(l.value0.value0,l.value0.value1,l.value0.value2,new pe(g,l.value0.value3,l.value0.value4,l.value0.value5.value0),l.value0.value5.value1,l.value0.value5.value2,new pe(l.value0.value5.value3,l.value0.value5.value4,l.value0.value5.value5,l.value0.value5.value6)))):l.value0 instanceof qi&&l.value0.value3 instanceof We?(_=!0,rn(t)(l.value1)(new We(l.value0.value0,l.value0.value1,l.value0.value2,new pe(l.value0.value3.value0,l.value0.value3.value1,l.value0.value3.value2,l.value0.value3.value3),l.value0.value3.value4,l.value0.value3.value5,new pe(l.value0.value3.value6,l.value0.value4,l.value0.value5,g)))):(_=!0,lu("The impossible happened in partial function `up`."))}throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): "+[l.constructor.name])}for(;!_;)v=D(s,m);return v}},n=function(i){return function(m){var s=i,_=!1,v;function D(l,g){if(g instanceof pe&&g.value0 instanceof Vt&&g.value3 instanceof Vt)return _=!0,r(l)(Vt.value);if(g instanceof pe){s=new le(new oi(g.value0,g.value1,g.value2),l),m=g.value3;return}if(g instanceof We&&g.value0 instanceof Vt&&g.value3 instanceof Vt&&g.value6 instanceof Vt)return _=!0,r(new le(new oi(Vt.value,g.value1,g.value2),l))(Vt.value);if(g instanceof We){s=new le(new qi(g.value0,g.value1,g.value2,g.value3,g.value4,g.value5),l),m=g.value6;return}return _=!0,lu("The impossible happened in partial function `removeMaxNode`.")}for(;!_;)v=D(s,m);return v}},a=function(i){var m=!1,s;function _(v){if(v instanceof pe&&v.value3 instanceof Vt)return m=!0,{key:v.value1,value:v.value2};if(v instanceof pe){i=v.value3;return}if(v instanceof We&&v.value6 instanceof Vt)return m=!0,{key:v.value4,value:v.value5};if(v instanceof We){i=v.value6;return}return m=!0,lu("The impossible happened in partial function `maxNode`.")}for(;!m;)s=_(i);return s},u=te(t),f=function(i){return function(m){var s=i,_=!1,v;function D(l,g){if(g instanceof Vt)return _=!0,z.value;if(g instanceof pe){var nt=u(e)(g.value1);if(g.value3 instanceof Vt&&nt instanceof be)return _=!0,new N(new ut(g.value2,r(l)(Vt.value)));if(nt instanceof be){var st=a(g.value0);return _=!0,new N(new ut(g.value2,n(new le(new Gi(st.key,st.value,g.value3),l))(g.value0)))}if(nt instanceof Zt){s=new le(new Gi(g.value1,g.value2,g.value3),l),m=g.value0;return}s=new le(new oi(g.value0,g.value1,g.value2),l),m=g.value3;return}if(g instanceof We){var me=function(){return g.value0 instanceof Vt&&g.value3 instanceof Vt&&g.value6 instanceof Vt}(),nt=u(e)(g.value4),Qt=u(e)(g.value1);if(me&&Qt instanceof be)return _=!0,new N(new ut(g.value2,rn(t)(l)(new pe(Vt.value,g.value4,g.value5,Vt.value))));if(me&&nt instanceof be)return _=!0,new N(new ut(g.value5,rn(t)(l)(new pe(Vt.value,g.value1,g.value2,Vt.value))));if(Qt instanceof be){var st=a(g.value0);return _=!0,new N(new ut(g.value2,n(new le(new Ui(st.key,st.value,g.value3,g.value4,g.value5,g.value6),l))(g.value0)))}if(nt instanceof be){var st=a(g.value3);return _=!0,new N(new ut(g.value5,n(new le(new $o(g.value0,g.value1,g.value2,st.key,st.value,g.value6),l))(g.value3)))}if(Qt instanceof Zt){s=new le(new Ui(g.value1,g.value2,g.value3,g.value4,g.value5,g.value6),l),m=g.value0;return}if(Qt instanceof de&&nt instanceof Zt){s=new le(new $o(g.value0,g.value1,g.value2,g.value4,g.value5,g.value6),l),m=g.value3;return}s=new le(new qi(g.value0,g.value1,g.value2,g.value3,g.value4,g.value5),l),m=g.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): "+[g.constructor.name])}for(;!_;)v=D(s,m);return v}};return f(wr.value)}},ka={foldr:function(t){return function(e){return function(r){if(r instanceof Vt)return e;if(r instanceof pe)return rr(ka)(t)(t(r.value2)(rr(ka)(t)(e)(r.value3)))(r.value0);if(r instanceof We)return rr(ka)(t)(t(r.value2)(rr(ka)(t)(t(r.value5)(rr(ka)(t)(e)(r.value6)))(r.value3)))(r.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): "+[r.constructor.name])}}},foldl:function(t){return function(e){return function(r){if(r instanceof Vt)return e;if(r instanceof pe)return dr(ka)(t)(t(dr(ka)(t)(e)(r.value0))(r.value2))(r.value3);if(r instanceof We)return dr(ka)(t)(t(dr(ka)(t)(t(dr(ka)(t)(e)(r.value0))(r.value2))(r.value3))(r.value5))(r.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): "+[r.constructor.name])}}},foldMap:function(t){return function(e){return function(r){if(r instanceof Vt)return Ot(t);if(r instanceof pe)return gt(t.Semigroup0())(Pn(ka)(t)(e)(r.value0))(gt(t.Semigroup0())(e(r.value2))(Pn(ka)(t)(e)(r.value3)));if(r instanceof We)return gt(t.Semigroup0())(Pn(ka)(t)(e)(r.value0))(gt(t.Semigroup0())(e(r.value2))(gt(t.Semigroup0())(Pn(ka)(t)(e)(r.value3))(gt(t.Semigroup0())(e(r.value5))(Pn(ka)(t)(e)(r.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): "+[r.constructor.name])}}}},ua={foldrWithIndex:function(t){return function(e){return function(r){if(r instanceof Vt)return e;if(r instanceof pe)return Fo(ua)(t)(t(r.value1)(r.value2)(Fo(ua)(t)(e)(r.value3)))(r.value0);if(r instanceof We)return Fo(ua)(t)(t(r.value1)(r.value2)(Fo(ua)(t)(t(r.value4)(r.value5)(Fo(ua)(t)(e)(r.value6)))(r.value3)))(r.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 147, column 26 - line 150, column 120): "+[r.constructor.name])}}},foldlWithIndex:function(t){return function(e){return function(r){if(r instanceof Vt)return e;if(r instanceof pe)return Qu(ua)(t)(t(r.value1)(Qu(ua)(t)(e)(r.value0))(r.value2))(r.value3);if(r instanceof We)return Qu(ua)(t)(t(r.value4)(Qu(ua)(t)(t(r.value1)(Qu(ua)(t)(e)(r.value0))(r.value2))(r.value3))(r.value5))(r.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 151, column 26 - line 154, column 120): "+[r.constructor.name])}}},foldMapWithIndex:function(t){return function(e){return function(r){if(r instanceof Vt)return Ot(t);if(r instanceof pe)return gt(t.Semigroup0())(ei(ua)(t)(e)(r.value0))(gt(t.Semigroup0())(e(r.value1)(r.value2))(ei(ua)(t)(e)(r.value3)));if(r instanceof We)return gt(t.Semigroup0())(ei(ua)(t)(e)(r.value0))(gt(t.Semigroup0())(e(r.value1)(r.value2))(gt(t.Semigroup0())(ei(ua)(t)(e)(r.value3))(gt(t.Semigroup0())(e(r.value4)(r.value5))(ei(ua)(t)(e)(r.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 155, column 26 - line 158, column 128): "+[r.constructor.name])}}},Foldable0:function(){return ka}},gy=function(){return Fo(ua)(function(t){return function(e){return function(r){return new le(t,r)}}})(wr.value)}();var Ap=function(){return Vt.value}();var Hm=function(t){return function(e){return function(r){return Kt(r)(tn)(bF(t)(e)(r))}}};var yp=function(t){return function(e){return function(r){return function(n){var a=e(dF(t)(r)(n));if(a instanceof z)return Hm(t)(r)(n);if(a instanceof N)return xl(t)(r)(a.value0)(n);throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): "+[a.constructor.name])}}}};var AF=function(t){return function(e){return function(r){return function(n){var a=function(u){return function(f){return function(i){return yp(t)(function(){var m=Kt(i)(e(i));return function(s){return N.create(m(s))}}())(u)(f)}}};return Qu(ua)(a)(n)(r)}}}};var Cy=function(t){return AF(t)(T)};var $l=function(t){return t.partitionMap};var Hi=function(t){return t.filterMap};var Ml=function(t){return t.filter};var ga={dimap:function(t){return function(e){return function(r){return function(n){return e(r(t(n)))}}}}},Mo=function(t){return t.dimap},Yu=function(t){return function(e){return Mo(t)(e)(Z(tt))}};var SF=function(t){return function(e){return function(r){return Cy(t)(e)(r)}}};var Jm=function(t){return gy(t)};var $y=function(t){return yy(t)(void 0)};var jm=function(t){return{append:SF(t)}};var My=function(t){return ky(t)},wy=function(t){return function(e){return function(r){return xl(t)(e)(void 0)(r)}}};var Py={foldMap:function(t){return function(e){var r=Pn(La)(t)(e);return function(n){return r(Jm(n))}}},foldl:function(t){return function(e){var r=dr(La)(t)(e);return function(n){return r(Jm(n))}}},foldr:function(t){return function(e){var r=rr(La)(t)(e);return function(n){return r(Jm(n))}}}};var Xm=Ap;var Oy=function(t){return{mempty:Xm,Semigroup0:function(){return jm(t)}}};var kp=function(t){return function(e){return function(r){return Hm(t)(e)(r)}}};function Iy(t){return function(e){return function(){return setTimeout(e,t)}}}function Ry(t){return function(){clearTimeout(t)}}var gp=Iy;var TF={eq:function(t){return function(e){return t===e}}},Cp={compare:function(t){return function(e){return te(qr)(t)(e)}},Eq0:function(){return TF}};var Pl=Ry;var Zu=function(t){return t.sampleOn};var hr=function(t){return t.keepLatest},$u=function(t){return t.fold};var Ol=function(t){return function(e){return function(r){return function(n){return Hi(t.Filterable1())(tn)($u(t)(function(a){return function(u){return p(Yo)(h(go))(e(a)(u.value0))}})(r)(new ut(n,z.value)))}}}},Ep=function(t){return function(e){var r=function(n){return function(a){if(a instanceof z)return new N({now:n,last:z.value});if(a instanceof N)return new N({now:n,last:new N(a.value0.now)});throw new Error("Failed pattern match at FRP.Event.Class (line 56, column 3 - line 56, column 50): "+[n.constructor.name,a.constructor.name])}};return Hi(t.Filterable1())(Z(tt))($u(t)(r)(e)(z.value))}},Il=function(t){return t.fix};var Sn=function(t){return function(e){return function(r){return I(t.Plus0().Alt0())(Zu(t)(e)(r))(Zu(t)(r)(p(t.Filterable1().Functor1())(Vf)(e)))}}},K=function(t){return t.bang};function Km(t){return function(e){return t===e}}var Ym=Km;var MF=function(t){return t};var xt=function(t){return function(e){return t(e)}},wF=function(t){return function(e){return function(r){return function(n){return function(a){return U(t.Monad0().Bind1())(Fe(t)(ra(z.value)))(function(u){return U(t.Monad0().Bind1())(r(function(f){return Fe(t)(Ae(Ii)(wa(new N(f))(u)))}))(function(f){return U(t.Monad0().Bind1())(n(function(i){return U(t.Monad0().Bind1())(Fe(t)(wn(u)))(pr(e)(Ve)(function(m){return a(i(m))}))}))(function(i){return h(e)(j(e.Apply0())(f)(i))})})})}}}}},Wt=MF,PF=function(t){return function(e){return function(r){return U(t.Monad0().Bind1())(Fe(t)(ra(z.value)))(function(n){return U(t.Monad0().Bind1())(e(function(a){return er(Xe)(t.Monad0().Bind1())(U(t.Monad0().Bind1())(Fe(t)(wn(n)))(ap(t.Monad0().Applicative0())(Ve)))(function(){return U(t.Monad0().Bind1())(xt(a)(r))(function(u){return Fe(t)(Ae(Ii)(wa(new N(u))(n)))})})}))(function(a){return h(t.Monad0().Applicative0())(er(Xe)(t.Monad0().Bind1())(U(t.Monad0().Bind1())(Fe(t)(wn(n)))(ap(t.Monad0().Applicative0())(Ve)))(function(){return a}))})})}}},k={map:function(t){return function(e){return function(r){return e(function(n){return r(t(n))})}}}};var OF=function(t){return function(e){return function(r){return function(n){return function(a){return U(t.Monad0().Bind1())(Fe(t)(ra(n)))(function(u){return r(function(f){return U(t.Monad0().Bind1())(Fe(t)(Xu(e(f))(u)))(a)})})}}}}},Rl=function(t){return function(e){return function(r){return function(n){return r(function(a){var u=e(a);if(u instanceof N)return n(u.value0);if(u instanceof z)return h(t)(void 0);throw new Error("Failed pattern match at FRP.Event (line 126, column 13 - line 128, column 27): "+[u.constructor.name])})}}}},Zm=function(t){return function(e){return Rl(t)(function(r){var n=e(r);if(n)return new N(r);if(!n)return z.value;throw new Error("Failed pattern match at FRP.Event (line 84, column 13 - line 86, column 25): "+[n.constructor.name])})}},Mu=function(t){return function(e){return Wt(function(r){return function(){var a=cr(Ot(Oy(Cp)))(),u=xt(e)(function(f){return function(){var m=cr(z.value)(),s=gp(t)(function(){r(f)();var v=Mr(m)();return Kt(h(c)(void 0))(function(D){return Df(kp(Cp)(D))(a)})(v)()})();return En(new N(s))(m)(),Df(gt(jm(Cp))($y(s)))(a)()}})();return function(){var i=Mr(a)();return na(c)(Py)(i)(Pl)(),u()}}})}};var Nl=function(t){return function(e){return U(t.Monad0().Bind1())(Fe(t)(ra([])))(function(r){return h(t.Monad0().Applicative0())({event:function(n){return U(e.Monad0().Bind1())(Fe(e)(Xu(function(a){return gt(ba)(a)([n])})(r)))(function(){return h(e.Monad0().Applicative0())(U(e.Monad0().Bind1())(Fe(e)(Xu(Rm(Ym)(n))(r)))(function(){return h(e.Monad0().Applicative0())(void 0)}))})},push:function(n){return U(e.Monad0().Bind1())(Fe(e)(wn(r)))(pr(e.Monad0().Applicative0())(Xt)(function(a){return a(n)}))}})})}},IF=function(t){return function(e){return function(r){return function(n){return U(e.Bind1())(Nl(t)(t))(function(a){var u=r(a.event);return U(e.Bind1())(xt(u.input)(a.push))(function(f){return U(e.Bind1())(xt(u.output)(n))(function(i){return h(e.Applicative0())(j(e.Bind1().Apply0())(f)(i))})})})}}}},zi=function(t){return function(e){return function(r){return Wt(function(n){return U(t.Monad0().Bind1())(Nl(t)(t))(function(a){return er(Xe)(t.Monad0().Bind1())(n(r(a.event)))(function(){return xt(e)(a.push)})})})}}},Ly=function(t){return{compact:Rl(t)(Z(tt)),separate:function(e){return{left:Rl(t)(function(r){if(r instanceof Jt)return new N(r.value0);if(r instanceof jt)return z.value;throw new Error("Failed pattern match at FRP.Event (line 67, column 13 - line 69, column 33): "+[r.constructor.name])})(e),right:Rl(t)(function(r){if(r instanceof jt)return new N(r.value0);if(r instanceof Jt)return z.value;throw new Error("Failed pattern match at FRP.Event (line 74, column 13 - line 76, column 32): "+[r.constructor.name])})(e)}}}},pu=function(t){return{filter:Zm(t),filterMap:Rl(t),partition:function(e){return function(r){return{yes:Zm(t)(e)(r),no:Zm(t)(function(){var n=fu(Ia);return function(a){return n(e(a))}}())(r)}}},partitionMap:function(e){return function(r){return{left:Hi(pu(t))(function(){var n=Pa(N.create)(T(z.value));return function(a){return n(e(a))}}())(r),right:Hi(pu(t))(function(n){return Q_(e(n))})(r)}}},Compactable0:function(){return Ly(t)},Functor1:function(){return k}}},jr=function(t){return function(e){return Wt(function(r){return U(t.Monad0().Bind1())(Nl(t)(t))(function(n){return er(Xe)(t.Monad0().Bind1())(r(e(n.push)(n.event)))(function(){return h(t.Monad0().Applicative0())(h(t.Monad0().Applicative0())(void 0))})})})}},wt=function(t){return function(e){return function(r){return p(t.Apply0().Functor0())(function(n){return h(t)(void 0)})(r(e))}}},O=function(t){return{alt:function(e){return function(r){return function(n){return Gt(t.Apply0())(p(t.Apply0().Functor0())(function(a){return function(u){return j(t.Apply0())(a)(u)}})(e(n)))(r(n))}}},Functor0:function(){return k}}},C=function(t){return{empty:function(e){return h(t)(h(t)(void 0))},Alt0:function(){return O(t)}}},S=function(t){return{fold:OF(t),keepLatest:PF(t),sampleOn:wF(t)(t.Monad0().Applicative0()),fix:IF(t)(t.Monad0()),bang:wt(t.Monad0().Applicative0()),Plus0:function(){return C(t.Monad0().Applicative0())},Filterable1:function(){return pu(t.Monad0().Applicative0())}}};var Sp="_____$__$_$$_vbus";function tv(t){return t[Sp]=Sp,t}function ev(t){return()=>{for(let e in t)delete t[e]}}function rv(t){return()=>{let e=(u,f,i,m)=>{let s=Object.keys(m);for(var _=0;_<s.length;_++)if(m[s[_]]instanceof Object&&m[s[_]][Sp]===Sp){let v={},D={};e(u,v,D,m[s[_]]),f[s[_]]=v,i[s[_]]=D}else{let v=`${Math.random()}`;u[v]={},f[s[_]]=D=>()=>{let l=Object.keys(u[v]);for(var g=0;g<l.length;g++)u[v][l[g]](D)()},i[s[_]]=D=>()=>{let l=`${Math.random()}`;return u[v][l]=D,()=>{delete u[v][l]}}}},r={},n={},a={};return e(r,n,a,t),{p:n,e:a,s:r}}}function Ll(t,e){var r={};for(var n in e)({}).hasOwnProperty.call(e,n)&&(r[n]=e[n]);for(var a in t)({}).hasOwnProperty.call(t,a)&&(r[a]=t[a]);return r}var By=function(t){return function(){return function(){return function(e){return function(r){return function(n){return Gu(Sr(t)(e))(r)(n)}}}}}};var Gy=function(){return function(){return function(t){return function(e){return Ll(t,e)}}}},Wl=function(t){return function(){return function(){return function(e){return function(r){return function(n){return Gu(Sr(t)(e))(r)(n)}}}}}},ii=function(t){return function(){return function(e){return function(r){return qa(Sr(t)(e))(r)}}}};var In={vb:function(t){return function(e){return function(r){return{}}}}},hp=function(t){return t.vb},su=function(){return function(t){return function(e){return function(r){return function(n){var a=hp(e)(d.value)(d.value)(d.value);return Wt(function(u){return U(t.Monad0().Bind1())(rv(a))(function(f){return er(Xe)(t.Monad0().Bind1())(u(n(f.p)(f.e)))(function(){return h(t.Monad0().Applicative0())(ev(f.s))})})})}}}}},wu=function(t){return function(){return function(){return function(){return function(e){return function(r){return function(){return function(){return function(){return function(){return{vb:function(n){return function(a){return function(u){return Wl(t)()()(d.value)(tv(hp(e)(d.value)(d.value)(d.value)))(hp(r)(d.value)(d.value)(d.value))}}}}}}}}}}}}}},nr=function(t){return function(){return function(){return function(e){return function(){return function(){return function(){return function(){return{vb:function(r){return function(n){return function(a){return Wl(t)()()(d.value)(void 0)(hp(e)(d.value)(d.value)(d.value))}}}}}}}}}}}};var Ji=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),av=function(){function t(){}return t.value=new t,t}();var Bl=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Gl=function(){function t(){}return t.value=new t,t}(),Uy=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Tp=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),gf=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),oc=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),w=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var qy=function(t){return t};var xp={eq:function(t){return function(e){return t instanceof Ji&&e instanceof Ji?t.value0===e.value0:t instanceof av&&e instanceof av}}};var B=function(t){return new gf(t)},et=function(t){return new oc(t)},uv=function(t){return new Tp(t)};var wo=function(t){return t.reflectType};var Jy={map:function(t){return function(e){return p(xe)(t)(e)}}};var qF=function(t){return wo(t)},Cf=function(){return function(t){return t}};var jy=function(t){return[t]};var Xy=function(){return function(){return function(){return function(){return function(){return function(t){return function(e){return function(r){return r[qF(t)(e)]}}}}}}}};var ov=[];var Ef=function(){return function(){return function(t){return function(e){return Li(t)(e)}}}};function Qy(t){return function(){var e={};for(var r in t)hasOwnProperty.call(t,r)&&(e[r]=t[r]);return e}}var Po={};function fv(t){return t()}function Ky(t,e){var r={};for(var n in t)hasOwnProperty.call(t,n)&&(r[n]=e(t[n]));return r}function Yy(t,e){var r={};for(var n in t)hasOwnProperty.call(t,n)&&(r[n]=e(n)(t[n]));return r}function Zy(t){return function(e){return function(r){return function(n){var a=r;function u(i){return function(m){return e(m)(i)(n[i])}}for(var f in n)hasOwnProperty.call(n,f)&&(a=t(a)(u(f)));return a}}}}function Ul(t){return function(e){var r=[];for(var n in e)hasOwnProperty.call(e,n)&&r.push(t(n)(e[n]));return r}}var HF=Object.keys||Ul(function(t){return function(){return t}});function cv(t){return function(e){return function(r){return function(){return r[t]=e,r}}}}var lv=function(t){return function(e){return function(){return delete e[t],e}}};var _v=Ul(function(t){return function(e){return e}});var KF=Qy;var ek=function(t){return function(e){return fv(function(){var n=KF(e)();return t(n)(),n})}};var rk=function(t){return function(e){return Yy(e,t)}};var ci=function(t){return function(e){return ek(cv(t)(e))}},Fp={map:function(t){return function(e){return Ky(e,t)}}},YF={mapWithIndex:rk,Functor0:function(){return Fp}},pv=function(){return ct};var $p=Zy(Vf),nk=function(t){return function(e){return $p(function(r){return function(n){return function(a){return gt(t.Semigroup0())(r)(e(n)(a))}}})(Ot(t))}},ql={foldl:function(t){return $p(function(e){return function(r){return t(e)}})},foldr:function(t){return function(e){return function(r){return rr(Xt)(t)(e)(_v(r))}}},foldMap:function(t){return function(e){return nk(t)(T(e))}}},ak={foldlWithIndex:function(t){return $p(Tt(t))},foldrWithIndex:function(t){return function(e){return function(r){return rr(Xt)(Zf(t))(e)(Ul(ut.create)(r))}}},foldMapWithIndex:function(t){return nk(t)},Foldable0:function(){return ql}},ZF={traverseWithIndex:function(t){return function(e){return function(r){return $p(function(n){return function(a){return function(u){return Gt(t.Apply0())(p(t.Apply0().Functor0())(Tt(ci(a)))(n))(e(a)(u))}}})(h(t)(Po))(r)}}},FunctorWithIndex0:function(){return YF},FoldableWithIndex1:function(){return ak},Traversable2:function(){return ic}},ic={traverse:function(t){var e=Bi(ZF)(t);return function(r){return e(T(r))}},sequence:function(t){return jn(ic)(t)(Z(tt))},Functor0:function(){return Fp},Foldable1:function(){return ql}};var sv=function(t){return ek(lv(t))};var uk=function(){function t(){}return t.value=new t,t}(),mv=function(){function t(){}return t.value=new t,t}(),t$=function(){function t(){}return t.value=new t,t}();var vv=function(t){return function(e){return function(r){var n=function(a){var u=function(f){return function(i){return new ut(i+1|0,new ut(f,i))}};return Ol(S(t))(u)(a)(0)};return new Tp(hr(S(t))(zi(t)(n(r))(function(a){return p(k)(function(u){return I(O(t.Monad0().Applicative0()))(K(S(t))(new Bl(e(u.value0))))(p(k)(T(Gl.value))(Ml(pu(t.Monad0().Applicative0()))(function(){var f=Yt(wi)(u.value1+1|0);return function(i){return f(tn(i))}}())(a)))})(a)})))}}},to=function(t){return function(e){return function(r){return function(n){return function(a){var u=function(f){return f(n)(a)};return function(f){if(f instanceof gf)return _n(Xt)(C(t))(to(t)(e)(r)(n)(a))(f.value0);if(f instanceof oc)return hr(S(e))(p(k)(to(t)(e)(r)(n)(a))(f.value0));if(f instanceof w)return u(r.toElt(f.value0));if(f instanceof Tp)return Wt(function(i){return U(e.Monad0().Bind1())(Fe(e)(ra(Po)))(function(m){return U(e.Monad0().Bind1())(xt(f.value0)(function(s){return U(e.Monad0().Bind1())(r.ids(a))(function(_){return U(e.Monad0().Bind1())(Fe(e)(ra(h(t)(void 0))))(function(v){return U(e.Monad0().Bind1())(r.ids(a))(function(D){return U(e.Monad0().Bind1())(Fe(e)(ra(h(t)(void 0))))(function(l){return U(e.Monad0().Bind1())(Fe(e)(ra(z.value)))(function(g){return U(e.Monad0().Bind1())(Fe(e)(ra(h(t)(void 0))))(function(nt){return U(e.Monad0().Bind1())(p(t.Apply0().Functor0())(Ji.create)(r.ids(a)))(function(st){return U(e.Monad0().Bind1())(Fe(e)(ra(uk.value)))(function(me){return U(e.Monad0().Bind1())(xt(s)(function(Qt){return U(e.Monad0().Bind1())(Fe(e)(wn(me)))(function(Je){return Qt instanceof Uy&&Je instanceof mv?U(e.Monad0().Bind1())(Fe(e)(wn(g)))(pr(t)(Ve)(function(){var vr=r.doLogic(Qt.value0)(a);return function(_t){return i(vr(_t))}}())):Qt instanceof Gl&&Je instanceof mv?er(Xe)(e.Monad0().Bind1())(Ae(t.Apply0().Functor0())(Fe(e)(wa(t$.value)(me))))(function(){var vr=j(t.Apply0())(j(t.Apply0())(j(t.Apply0())(j(t.Apply0())(U(e.Monad0().Bind1())(Fe(e)(wn(g)))(pr(t)(Ve)(function(_t){return na(t)(Ve)(n.parent)(function(ve){return i(r.disconnectElement(a)({id:_t,parent:ve,scope:st}))})})))(Ha(e.Monad0().Bind1())(Fe(e)(wn(v)))))(Ha(e.Monad0().Bind1())(Fe(e)(wn(l)))))(Ae(t.Apply0().Functor0())(Fe(e)(Xu(sv(_))(m)))))(Ae(t.Apply0().Functor0())(Fe(e)(Xu(sv(D))(m))));return j(t.Apply0())(Ae(t.Apply0().Functor0())(Fe(e)(wa(vr)(nt))))(vr)}):Qt instanceof Bl&&Je instanceof uk?er(Xe)(e.Monad0().Bind1())(Ae(t.Apply0().Functor0())(Fe(e)(wa(mv.value)(me))))(function(){return U(e.Monad0().Bind1())(xt(to(t)(e)(r)({parent:n.parent,scope:st,raiseId:function(vr){return Ae(t.Apply0().Functor0())(Fe(e)(wa(new N(vr))(g)))}})(a)(function(){return Qt.value0 instanceof w?Qt.value0:r.wrapElt(Qt.value0)}()))(i))(function(vr){return er(Xe)(e.Monad0().Bind1())(Ae(t.Apply0().Functor0())(Fe(e)(Xu(ci(D)(vr))(m))))(function(){return Ae(t.Apply0().Functor0())(Fe(e)(wa(vr)(l)))})})}):h(t)(void 0)})}))(function(Qt){return er(Xe)(e.Monad0().Bind1())(Ae(t.Apply0().Functor0())(Fe(e)(wa(Qt)(v))))(function(){return er(Xe)(e.Monad0().Bind1())(Ae(t.Apply0().Functor0())(Fe(e)(Xu(ci(_)(Qt))(m))))(function(){return Ha(e.Monad0().Bind1())(Fe(e)(wn(nt)))})})})})})})})})})})})}))(function(s){return h(t)(er(Xe)(e.Monad0().Bind1())(U(e.Monad0().Bind1())(Fe(e)(wn(m)))(dr(ql)(j(t.Apply0()))(h(t)(void 0))))(function(){return s}))})})});throw new Error("Failed pattern match at Bolson.Control (line 208, column 17 - line 293, column 20): "+[f.constructor.name])}}}}}},ok=function(t){return function(e){return function(r){return function(n){var a=function(u){return function(f){return Wt(function(i){return U(t.Monad0().Bind1())(Fe(t)(ra(z.value)))(function(m){var s=n(new w(r.fromElt(function(_){return function(v){return Wt(function(D){return er(Xe)(t.Monad0().Bind1())(U(t.Monad0().Bind1())(Fe(t)(wn(m)))(function(l){if(l instanceof z)return h(t.Monad0().Applicative0())(void 0);if(l instanceof N)return na(t.Monad0().Applicative0())(Ve)(_.parent)(function(g){return Mn(t.Monad0().Applicative0())(l.value0!==g)(j(t.Monad0().Bind1().Apply0())(_.raiseId(l.value0))(D(r.connectToParent(f)({id:l.value0,parent:g}))))});throw new Error("Failed pattern match at Bolson.Control (line 319, column 36 - line 326, column 16): "+[l.constructor.name])}))(function(){return h(t.Monad0().Applicative0())(h(t.Monad0().Applicative0())(void 0))})})}})));return xt(to(t.Monad0().Applicative0())(t)(e)({parent:u.parent,scope:u.scope,raiseId:function(_){return er(Xe)(t.Monad0().Bind1())(u.raiseId(_))(function(){return Ae(t.Monad0().Bind1().Apply0().Functor0())(Fe(t)(wa(new N(_))(m)))})}})(f)(s))(i)})})}};return new w(r.fromElt(a))}}}},e$=ct,r$=function(){return function(t){return function(e){return function(r){return function(n){return function(a){return function(u){return function(f){var i=function(m){return function(s){return Wt(function(_){return U(t.Monad0().Bind1())(Rb(p(xe)(T(""))(Cf()(u))))(function(v){var D=kr(Xt)(C(t.Monad0().Applicative0()))(To(Wi)(function(l){return sm(TA)(function(g){return function(nt){return nt instanceof w?function(st){return st({parent:z.value,scope:r(m.scope),raiseId:function(me){return Nb(l)(me)(v)}})(s)}(n.toElt(nt.value0)):g(n.wrapElt(nt))}})})(Cf()(u)));return U(t.Monad0().Bind1())(xt(D)(_))(function(l){return U(t.Monad0().Bind1())(Fe(t)(ra(h(t.Monad0().Applicative0())(void 0))))(function(g){return U(t.Monad0().Bind1())(p(t.Monad0().Bind1().Apply0().Functor0())(ct)(Lb(v)))(function(nt){var st=p(Jy)(function(Qt){return new w(a.fromElt(function(Je){return function(vr){return Wt(function(_t){return er(Xe)(t.Monad0().Bind1())(Je.raiseId(Qt))(function(){return er(Xe)(t.Monad0().Bind1())(na(t.Monad0().Applicative0())(Ve)(Je.parent)(function(ve){return _t(a.giveNewParent(vr)({id:Qt,parent:ve,scope:Je.scope}))}))(function(){return h(t.Monad0().Applicative0())(h(t.Monad0().Applicative0())(void 0))})})})}}))})(nt),me=to(t.Monad0().Applicative0())(t)(n)(m)(s)(e$(f(st)(ct)));return U(t.Monad0().Bind1())(xt(me)(_))(function(Qt){return er(Xe)(t.Monad0().Bind1())(Ae(t.Monad0().Bind1().Apply0().Functor0())(Fe(t)(wa(Qt)(g))))(function(){return h(t.Monad0().Applicative0())(er(Xe)(t.Monad0().Bind1())(l)(function(){return er(Xe)(t.Monad0().Bind1())(Mn(t.Monad0().Applicative0())(!e)(na(t.Monad0().Applicative0())(Xt)(Cf()(nt))(function(Je){return _(a.deleteFromCache(s)({id:Je}))})))(function(){return Ha(t.Monad0().Bind1())(Fe(t)(wn(g)))})}))})})})})})})})}};return new w(a.fromElt(i))}}}}}}}};var Dv=function(){return function(t){return function(e){return function(r){return function(n){return function(a){return r$()(t)(!1)(Z(tt))(e)(r)(n)(a)}}}}}};var n$=function(t){return t},Hl=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),zl=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),a$=function(t){return t},wp=Fu(),b=a$;var G=function(){return Hl.create}();var ft=function(){return zl.create}(),Ke=function(){var t=p(Ma)(p(x)(T(!0)));return function(e){return n$(t(e))}}(),Y=function(t){return t.attr};var o$=function(t){return t.makeText},i$=function(t){return function(e){return function(r){return p(k)(function(n){return t.setText(function(a){return{id:e,text:a}}(n))})(r)}}},f$=function(t){return function(e){return function(r){return p(k)(function(n){return function(a){if(a.value instanceof Hl)return t.setProp({id:e,key:a.key,value:a.value.value0});if(a.value instanceof zl)return t.setCb({id:e,key:a.key,value:a.value.value0});throw new Error("Failed pattern match at Deku.Control (line 67, column 26 - line 69, column 45): "+[a.value.constructor.name])}(wp(n))})(r)}}},c$=function(t){return t.makeElement},nn=function(t){return function(e){var r=function(n){return function(a){return Wt(function(u){return U(t.Bind1())(a.ids)(function(f){return er(Xe)(t.Bind1())(n.raiseId(f))(function(){return p(t.Bind1().Apply0().Functor0())(j(t.Bind1().Apply0())(u(a.deleteFromCache({id:f}))))(xt(kr(Xt)(C(t.Applicative0()))([wt(t.Applicative0())(o$(a)({id:f,parent:n.parent,scope:n.scope})),i$(a)(f)(e)]))(u))})})})}};return new w(r)}},ar=function(t){return function(e){return nn(t)(wt(t.Applicative0())(e))}},V=function(t){return function(e){return function(r){return function(n){var a=function(u){return function(f){return Wt(function(i){return U(t.Monad0().Bind1())(f.ids)(function(m){return er(Xe)(t.Monad0().Bind1())(u.raiseId(m))(function(){return p(t.Monad0().Bind1().Apply0().Functor0())(j(t.Monad0().Bind1().Apply0())(i(f.deleteFromCache({id:m}))))(xt(I(O(t.Monad0().Applicative0()))(kr(Xt)(C(t.Monad0().Applicative0()))([wt(t.Monad0().Applicative0())(c$(f)({id:m,parent:u.parent,scope:u.scope,tag:e})),f$(f)(m)(r)]))(ik(t)({parent:new N(m),scope:u.scope,raiseId:function(s){return h(t.Monad0().Applicative0())(void 0)}})(f)(n)))(i))})})})}};return a}}}},ik=function(t){return to(t.Monad0().Applicative0())(t)({doLogic:function(e){return function(r){return function(n){return r.sendToTop({id:n})}}},ids:function(){var e=Ar();return function(r){return function(n){return n.ids}(e(r))}}(),disconnectElement:function(e){return function(r){return e.disconnectElement({id:r.id,scope:r.scope,parent:r.parent,scopeEq:Yt(xp)})}},wrapElt:function(){var e=V(t)("div")(M(C(t.Monad0().Applicative0())));return function(r){return w.create(e(r))}}(),toElt:function(e){return e}})},l$=function(t){return function(e){return function(r){return function(n){return Wt(function(a){return U(t.Monad0().Bind1())(n.ids)(function(u){return xt(I(O(t.Monad0().Applicative0()))(wt(t.Monad0().Applicative0())(n.makeRoot({id:u,root:e})))(ik(t)({parent:new N(u),scope:new Ji("rootScope"),raiseId:function(f){return h(t.Monad0().Applicative0())(void 0)}})(n)(r)))(a)})})}}}};var fk=function(t){return function(e){return function(r){return l$(t)(e)(new gf(r))}}};var lr=function(){function t(){}return t.value=new t,t}();var sr={attr:function(t){return function(e){return b({key:"click",value:ft(e)})}}};var qt=function(){function t(){}return t.value=new t,t}();var Pp={attr:function(t){return function(e){return b({key:"style",value:G(e)})}}};var ck={attr:function(t){return function(e){return b({key:"style",value:G(e)})}}};var pt={attr:function(t){return function(e){return b({key:"style",value:G(e)})}}};var lk={attr:function(t){return function(e){return b({key:"style",value:G(e)})}}},fc={attr:function(t){return function(e){return b({key:"style",value:G(e)})}}};var dv={attr:function(t){return function(e){return b({key:"style",value:G(e)})}}};var _k={attr:function(t){return function(e){return b({key:"style",value:G(e)})}}};var bv=function(t){return function(e){return function(r){return new w(V(t)("a")(e)(B(r)))}}};var Se=function(t){return function(e){return function(r){return new w(V(t)("div")(e)(B(r)))}}},Ge=function(t){return Se(t)(M(C(t.Monad0().Applicative0())))};var lc=function(t){return function(e){return function(r){return new w(V(t)("span")(e)(B(r)))}}},Av=function(t){return lc(t)(M(C(t.Monad0().Applicative0())))};var yv=(t,e,r,n)=>{t(a=>n.units[a].main.appendChild(n.units[e].main))(r)},sk=t=>e=>r=>n=>()=>{var a,u=r.id;n.scopes[r.scope]||(n.scopes[r.scope]=[]),n.scopes[r.scope].push(u),t&&r.parent!=="@portal@"&&(a=document.body.querySelectorAll("[data-deku-ssr-"+u+"]").item(0))?n.units[u]={listeners:{},parent:r.parent,scope:r.scope,main:a}:(n.units[u]={listeners:{},parent:r.parent,scope:r.scope,main:document.createElement(r.tag)},yv(e,u,r.parent,n))},mk=t=>e=>r=>n=>()=>{var a=r.id,u;n.scopes[r.scope]||(n.scopes[r.scope]=[]),n.scopes[r.scope].push(a),t&&r.parent!=="@portal@"&&(u=document.body.querySelectorAll("[data-deku-ssr-"+r.parent+"]").item(0))?n.units[a]={main:u.childNodes[0],parent:r.parent,scope:r.scope}:(n.units[a]={main:document.createTextNode(""),parent:r.parent,scope:r.scope},yv(e,a,r.parent,n))};function kv(){return{units:{},scopes:{}}}var vk=t=>e=>r=>()=>{var n=e.id,a=e.value;t&&!r.units[n]&&(dom=document.body.querySelectorAll("[data-deku-ssr-"+n+"]").item(0))&&(r.units[n]={listeners:{},parent:e.parent,scope:e.scope,main:dom},r.scopes[e.scope]||(r.scopes[e.scope]=[]),r.scopes[e.scope].push(n)),r.units[n].main.tagName==="INPUT"&&e.key==="value"?r.units[n].main.value=a:r.units[n].main.tagName==="INPUT"&&e.key==="checked"?r.units[n].main.checked=a==="true":r.units[n].main.setAttribute(e.key,a)},Dk=t=>e=>r=>()=>{var n=e.id,a=e.value;if(t&&!r.units[n]&&(dom=document.body.querySelectorAll("[data-deku-ssr-"+n+"]").item(0))&&(r.units[n]={listeners:{},parent:e.parent,scope:e.scope,main:dom},r.scopes[e.scope]||(r.scopes[e.scope]=[]),r.scopes[e.scope].push(n)),e.key==="@self@")a(r.units[n].main)();else{r.units[n].listeners[e.key]&&r.units[n].main.removeEventListener(e.key,r.units[n].listeners[e.key]);var u=f=>a(f)();r.units[n].main.addEventListener(e.key,u),r.units[n].listeners[e.key]=u}},dk=t=>e=>()=>{var r=t.id;e.units[r].main.nodeValue=t.text},bk=t=>e=>r=>n=>()=>{var a,u,f=r.id,i=r.html,m=r.verb,s=r.cache,_=r.parent,v=r.scope,D=r.pxScope;if(t&&_!=="@portal@"&&(a=document.body.querySelectorAll("[data-deku-ssr-"+f+"]").item(0)))n.units[f]={listeners:{},scope:v,parent:_,main:a};else{let g=Object.entries(s);for(var l=0;l<g.length;l++){let nt=g[l][0];g[l][1]===!0?i=i.replace(m+nt+m,'data-deku-attr-internal="'+nt+'"'):i=i.replace(m+nt+m,'<span style="display:contents;" data-deku-elt-internal="'+nt+'"></span>')}u=document.createElement("div"),u.innerHTML=i.trim(),n.units[f]={listeners:{},scope:v,parent:_,main:u.firstChild}}n.scopes[v]||(n.scopes[v]=[]),n.scopes[v].push(f),u||(u=a),u.querySelectorAll("[data-deku-attr-internal]").forEach(function(g){var nt=g.getAttribute("data-deku-attr-internal");let st=nt+D;n.units[st]={listeners:{},main:g,scope:v},n.scopes[v].push(st)}),u.querySelectorAll("[data-deku-elt-internal]").forEach(function(g){var nt=g.getAttribute("data-deku-elt-internal");let st=nt+D;n.units[nt+D]={listeners:{},main:g,scope:v},n.scopes[v].push(st)}),a||yv(e,f,_,n)},Ak=t=>e=>()=>{var r=t.id;e.units[r]={main:t.root}},yk=t=>e=>()=>{var r=t.id,n=t.parent;e.units[r].containingScope=t.scope,e.units[n].main.prepend(e.units[r].main)},kk=t=>e=>()=>{var r=t.id;e.units[r].noop||e.units[r].containingScope&&!t.scopeEq(e.units[r].containingScope)(t.scope)||e.units[r].main.remove()},gk=t=>e=>()=>{delete e.units[t.id]},Ck=t=>e=>()=>{var r=t.id;e.units[r].main.parentNode.prepend(e.units[r].main)};var Ek=function(t){return function(e){return function(r){return(r|0)===r?t(r):e}}},qe=function(t){return t};var gv=function(t){return function(e){return Math.pow(t,e)|0}};var Op=isFinite;var Vl=Math.floor;var ji=function(t){return function(e){return Math.pow(t,e)}},Jl=function(t){return function(e){return t%e}},Ip=Math.round;var Rp=Math.sin;var Xi=3.141592653589793;var _c=function(){return Ek(N.create)(z.value)}(),hk=function(t){if(!Op(t))return 0;if(t>=qe(Hn(sf)))return Hn(sf);if(t<=qe(zn(sf)))return zn(sf);if(tr)return Aa(0)(_c(t));throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): "+[t.constructor.name])},Tk=function(t){return hk(Ip(t))};var jl=function(t){return hk(Vl(t))};var Ou=Math.random;var Xl=function(t){return function(e){return function(){var n=Ou(),a=(qe(e)-qe(t)+1)*n+qe(t);return jl(a)}}};var xk=function(t){return t};var b$=1,Np=2147483647,A$=function(){return Np-1|0}(),sc=function(t){var e=function(r){return function(n){return function(a){var u=n-r|0,f=uu(Eo)(a)(u),i=f<r;return i?f+n|0:f}}};return e(b$)(A$)(t)};var y$=0,k$=48271,Fk=function(t){return function(e){return Jn()(_c(Jl(qe(k$)*qe(e)+qe(t))(qe(Np))))}},$k=Fk(y$);var F$=function(){function t(f){this.fn=f}var e={},r=function(f,i){this.head=f,this.tail=i};function n(f){return new r(f,e)}function a(f){return function(i){return new r(f,i)}}function u(f){for(var i=[],m=f;m!==e;)i.push(m.head),m=m.tail;return i}return function(f){return function(i){return function(m){var s=function(v,D){return f(i(a)(m(v)))(D)},_=function(v,D,l){if(D===0)return v;var g=l[D-1];return new t(function(){var nt=_(s(g,v),D-1,l);return nt})};return function(v){for(var D=i(n)(m(v[v.length-1])),l=_(D,v.length-1,v);l instanceof t;)l=l.fn();return i(u)(l)}}}}}();var Ik=function(t){return t};var Rk=ba;var Nk=Xt;var Bk=Ik,Kl=function(t){return t};var Yl=function(t){return Bk(ay(t))};var mc=function(t){if(xu(t)>0)return new N(Bk(t));if(tr)return z.value;throw new Error("Failed pattern match at Data.Array.NonEmpty (line 157, column 1 - line 157, column 58): "+[t.constructor.name])};var Gk=function(t){return function(e){return t(Kl(e))}};var Uk=Gk(xu);var qk=function(){return Gk(Om())};var li=function(t){return t.state};function Io(t){return new Error(t)}function hf(t){return function(){throw t}}function Bp(t){return function(e){return function(){try{return e()}catch(r){return r instanceof Error||Object.prototype.toString.call(r)==="[object Error]"?t(r)():t(new Error(r.toString()))()}}}}var ro=function(t){return t.throwError};var lM={throwError:hf,Monad0:function(){return ae}};var Ov={catchError:Tt(Bp),MonadThrow0:function(){return lM}};var _i=function(t){return t.catchError};var t_=function(t){return function(e){return _i(t)(p(t.MonadThrow0().Monad0().Bind1().Apply0().Functor0())(jt.create)(e))(function(){var r=h(t.MonadThrow0().Monad0().Applicative0());return function(n){return r(Jt.create(n))}}())}};var ur={liftEffect:Z(tt),Monad0:function(){return ae}},mr=function(t){return t.liftEffect};var Rv=function(t){return{map:function(e){return function(r){return function(n){return p(t)(function(a){return new ut(e(a.value0),a.value1)})(r(n))}}}}};var Nv=function(t){return{Applicative0:function(){return Wv(t)},Bind1:function(){return Lv(t)}}},Lv=function(t){return{bind:function(e){return function(r){return function(n){return U(t.Bind1())(e(n))(function(a){var u=r(a.value0);return u(a.value1)})}}},Apply0:function(){return zp(t)}}},zp=function(t){return{apply:Uu(Nv(t)),Functor0:function(){return Rv(t.Bind1().Apply0().Functor0())}}},Wv=function(t){return{pure:function(e){return function(r){return h(t.Applicative0())(new ut(e,r))}},Apply0:function(){return zp(t)}}};var Kk=function(t){return{state:function(e){var r=h(t.Applicative0());return function(n){return r(e(n))}},Monad0:function(){return Nv(t)}}};var Zk=function(t){return function(e){var r=t(e);return r.value0}};var bM=function(t){return t};var eg=function(){var t=function(e){return new ut(xk(e.newSeed),function(){var r={};for(var n in e)({}).hasOwnProperty.call(e,n)&&(r[n]=e[n]);return r.newSeed=$k(e.newSeed),r}())};return li(Kk(Hu))(t)}();var no=Rv(Co),rg=p(no)(function(t){return qe(t)/qe(Np)})(eg);var dc=function(t){return Zk(bM(t))};var xf=Lv(Hu);var Ff=zp(Hu),tg=function(t){return function(e){var r=qe(e),n=qe(t),a=function(i){return n+Jl(i)(r-n+1)},u=p(no)(qe)(eg),f=Gt(Ff)(p(no)(Le(Da))(u))(p(no)($n(Da)(2))(u));return p(no)(function(i){return jl(a(i))})(f)}},Bv=function(t){return function(e){var r=t<=e;return r?tg(t)(e):tg(e)(t)}};var u_=Wv(Hu);var Gv=function(t){return U(xf)(Bv(0)(Uk(t)-1|0))(function(e){return h(u_)(qk()(t)(e))})};var i_=function(t){return t.arbitrary};var ng={arbitrary:rg};var ag=function(){return{arbitrary:Bv(-1e6)(1e6)}}();var ug=function(t){return{ids:function(){var r=Mr(t)(),n=zt(za)(dc(i_(ag))({newSeed:sc(r),size:5}));return Ae(x)(Xf(Le(Su)(1))(t))(),n},makeElement:sk(!1)(Kt(void 0)),makeRoot:Ak,makeText:mk(!1)(Kt(void 0)),makePursx:bk(!1)(Kt(void 0)),setProp:vk(!1),setCb:Dk(!1),setText:dk,sendToTop:Ck,deleteFromCache:gk,giveNewParent:yk,disconnectElement:kk}};var EM=function(t){return t};var Q=function(t){return{pursxToElement:function(e){return function(r){return function(n){return{cache:Po,element:function(a){return function(u){return M(C(t))}}}}}}}},qv=function(t){return t.pursxToElement},an=function(){return function(t){return function(e){return function(r){return function(n){return{pursxToElement:function(a){return function(u){return function(f){var i=qv(t)(a)(d.value)(f);return{cache:ci(wo(e)(d.value))(!0)(i.cache),element:function(m){return function(s){return I(O(n.Monad0().Applicative0()))(p(k)(Yu(ga)(wp)(function(_){if(_.value instanceof Hl)return s.setProp({id:wo(e)(d.value)+a,key:_.key,value:_.value.value0});if(_.value instanceof zl)return s.setCb({id:wo(e)(d.value)+a,key:_.key,value:_.value.value0});throw new Error("Failed pattern match at Deku.Pursx (line 4476, column 38 - line 4486, column 24): "+[_.value.constructor.name])}))(ii(r)()(d.value)(f)))(i.element(m)(s))}}}}}}}}}}}};var L=EM,_e=function(t){return function(e){return function(){return function(){return function(r){return function(n){return function(a){return function(u){return function(f){var i=function(m){return function(s){return Wt(function(_){return U(r.Monad0().Bind1())(s.ids)(function(v){return U(r.Monad0().Bind1())(s.ids)(function(D){return er(Xe)(r.Monad0().Bind1())(m.raiseId(v))(function(){var l=qv(n)(D)(d.value)(f);return p(r.Monad0().Bind1().Apply0().Functor0())(j(r.Monad0().Bind1().Apply0())(_(s.deleteFromCache({id:v}))))(xt(I(O(r.Monad0().Applicative0()))(wt(r.Monad0().Applicative0())(s.makePursx({id:v,parent:m.parent,cache:l.cache,pxScope:D,scope:m.scope,html:wo(t)(u),verb:wo(e)(a)})))(l.element(m)(s)))(_))})})})})}};return new w(i)}}}}}}}}},Ft=function(t){return function(){return function(){return function(e){return function(r){return _e(t)({reflectType:function(){return"~"}})()()(r)(e)(d.value)}}}}};var SM=function(t){return to(t.Monad0().Applicative0())(t)({doLogic:function(e){return function(r){return function(n){return r.sendToTop({id:n})}}},ids:function(){var e=Ar();return function(r){return function(n){return n.ids}(e(r))}}(),disconnectElement:function(e){return function(r){return e.disconnectElement({id:r.id,scope:r.scope,parent:r.parent,scopeEq:Yt(xp)})}},wrapElt:function(){var e=V(t)("div")(M(C(t.Monad0().Applicative0())));return function(r){return w.create(e(r))}}(),toElt:function(e){return e}})},W=function(){return function(t){return function(e){return function(r){return function(n){return{pursxToElement:function(a){return function(u){return function(f){var i=ii(r)()(d.value)(f),m=qv(t)(a)(d.value)(f);return{cache:ci(wo(e)(d.value))(!1)(m.cache),element:function(s){return function(_){return I(O(n.Monad0().Applicative0()))(SM(n)({parent:new N(wo(e)(d.value)+a),scope:s.scope,raiseId:function(v){return h(n.Monad0().Applicative0())(void 0)}})(_)(i))(m.element(s)(_))}}}}}}}}}}}};function og(t){var e={};for(var r in t)({}).hasOwnProperty.call(t,r)&&(e[r]=t[r]);return e}function ig(t){return function(e){return function(r){return r[t]=e,r}}}var Hv=Qo;var zv=function(){return function(){return function(t){return function(e){return function(r){return function(n){return ig(Sr(t)(e))(r)(n)}}}}}};var Vv=tt,fg=function(t){return function(e){return t(og(e))}},cg=Tt(fg)({});var bt=function(){return function(){return{defaults:Tt(Gy()())}}},TM=function(t){return t.defaults},At={convertRecordOptions:function(t){return function(e){return function(r){return Z(Vv)}}}},_g=function(t){return t.convertRecordOptions},oa=function(t){return t.convertOptionsWithDefaults},yt=function(){return function(t){return{convertOptions:function(e){return function(r){return cg(_g(t)(e)(d.value)(r))}}}}},xM=function(t){return t.convertOptions},kt=function(t){return function(e){return{convertOptionsWithDefaults:function(r){return function(n){var a=TM(e)(n),u=xM(t)(r);return function(f){return a(u(f))}}}}}},FM=function(t){return t.convertOption},J=function(t){return function(e){return function(){return function(){return function(){return function(r){return{convertRecordOptions:function(n){return function(a){return function(u){return yo(Hv)(zv()()(r)(d.value)(FM(e)(n)(d.value)(ii(r)()(d.value)(u))))(_g(t)(n)(d.value)(u))}}}}}}}}}};var MM=function(){return function(){return function(){return function(t){return function(e){return function(r){return z_(r.type)(t)?qa(r.type)(t)(r.value):e(r)}}}}}};var _r=function(){return function(t){return function(e){return function(r){return{type:Sr(t)(e),value:r}}}}};var wM=function(t){return lu("Data.Variant: pattern match failure ["+(t.type+"]"))},Nr=function(){return function(){return function(){return function(t){return MM()()()(t)(wM)}}}};var Jv=function(){var t=sp(Wm);return function(e){return mp(t(e))}}();var Cz=typeof Array.from=="function",Ez=typeof Symbol<"u"&&Symbol!=null&&typeof Symbol.iterator<"u"&&typeof String.prototype[Symbol.iterator]=="function",Sz=typeof String.prototype.fromCodePoint=="function",hz=typeof String.prototype.codePointAt=="function";var pi={proof:function(t){return t},Coercible0:function(){}},Xv=function(t){return t.proof};var Du=void 0;var Zp=function(t){return t.toInt},bg=function(t){return function(e){return Zp(t)(Du)}};var Va={toInt:function(t){return 8}},Ag={Nat0:function(){return Va}},No={toInt:function(t){return 7}},yg={Nat0:function(){return No}},Lo={toInt:function(t){return 6}},kg={Nat0:function(){return Lo}},Ea={toInt:function(t){return 5}},ts={Nat0:function(){return Ea}},Rn={toInt:function(t){return 4}},Kn={Nat0:function(){return Rn}},Nn={toInt:function(t){return 3}},du={Nat0:function(){return Nn}},Ln={toInt:function(t){return 2}},bu={Nat0:function(){return Ln}},Wn={toInt:function(t){return 1}},Au={Nat0:function(){return Wn}},Tr={toInt:function(t){return 0}};var Me=function(t){return function(){return function(e){return function(){return function(r){return{Nat0:e.Nat1,Pos1:function(){return t}}}}}}};var ao={Nat0:function(){return No},Nat1:function(){return Va}};var uo={Nat0:function(){return Lo},Nat1:function(){return Va}};var oo={Nat0:function(){return Ea},Nat1:function(){return Va}};var io={Nat0:function(){return Rn},Nat1:function(){return Va}};var ia={Nat0:function(){return Rn},Nat1:function(){return Ea}};var fo={Nat0:function(){return Nn},Nat1:function(){return Va}};var fa={Nat0:function(){return Nn},Nat1:function(){return Ea}};var co={Nat0:function(){return Ln},Nat1:function(){return Va}};var ca={Nat0:function(){return Ln},Nat1:function(){return Ea}};var lo={Nat0:function(){return Wn},Nat1:function(){return Va}};var la={Nat0:function(){return Wn},Nat1:function(){return Ea}};var _o={Nat0:function(){return Tr},Nat1:function(){return Va}};var _a={Nat0:function(){return Tr},Nat1:function(){return Ea}};var gg={Nat0:function(){return Tr},Nat1:function(){return Va}};var Qv={Nat0:function(){return Tr},Nat1:function(){return No}};var Kv={Nat0:function(){return Tr},Nat1:function(){return Lo}};var c_={Nat0:function(){return Tr},Nat1:function(){return Ea}};var Wa={Nat0:function(){return Tr},Nat1:function(){return Rn}};var sn={Nat0:function(){return Tr},Nat1:function(){return Nn}};var mn={Nat0:function(){return Tr},Nat1:function(){return Ln}};var vn={Nat0:function(){return Tr},Nat1:function(){return Wn}},yu={Nat0:function(){return Tr},Nat1:function(){return Tr}};var Cg=Zo;var es=function(t){return t};var l_=function(t){return function(){return function(e){return function(r){return e[Zp(t)(r)]}}}};var rs=function(t){return function(e){var r=bg(t)(d.value),n=function(){return r===0?[]:ln(0)(r-1|0)}();return p(xe)(e)(n)}};var Ru=[];var we=function(t){return function(e){return function(r){return Li(e)(r)}}};var Dn={first:function(t){return function(e){return new ut(t(e.value0),e.value1)}},second:p(Yo),Profunctor0:function(){return ga}},Bn=function(t){return t.second},ns=function(t){return t.first};var cw=function(t){return function(e){return function(r){return function(n){return Mo(r)(t)(e)(n)}}}};var Tg=function(){return function(){return function(t){return cw(Fu())(Fu())(t)}}};var xg=function(){return function(){return function(t){return Tg()()(t)}}};var pw=function(t){return function(e){return function(r){return Mo(e.Profunctor0())(t)(function(n){return n.value1(n.value0)})(ns(e)(r))}}},Fg=function(t){return function(e){return function(r){return pw(function(n){return new ut(t(n),function(a){return e(n)(a)})})(r)}}};var $g=function(t){return function(){return function(){return function(e){return function(r){return Fg(ii(t)()(e))(Tt(By(t)()()(e)))(r)}}}}};var Mg=function(t){return t};var dw=JSON.parse;var bw=JSON.stringify;var as=function(t){return t};var us=function(t){return t};var os=function(t){return function(e){return t(e)}},__=function(t){return{map:function(e){return os(p(t)(p(vf)(e)))}}};var tD=function(t){return{Applicative0:function(){return p_(t)},Bind1:function(){return eD(t)}}},eD=function(t){return{bind:function(e){return function(r){return U(t.Bind1())(e)(Pa(function(){var n=h(t.Applicative0());return function(a){return n(Jt.create(a))}}())(function(n){var a=r(n);return a}))}},Apply0:function(){return Pg(t)}}},Pg=function(t){return{apply:Uu(tD(t)),Functor0:function(){return __(t.Bind1().Apply0().Functor0())}}},p_=function(t){return{pure:function(){var e=h(t.Applicative0());return function(r){return as(e(jt.create(r)))}}(),Apply0:function(){return Pg(t)}}};var Og=function(t){return{throwError:function(){var e=h(t.Applicative0());return function(r){return as(e(Jt.create(r)))}}(),Monad0:function(){return tD(t)}}};var rD=function(t){return function(e){return{alt:function(r){return function(n){return U(e.Bind1())(r)(function(a){if(a instanceof jt)return h(e.Applicative0())(new jt(a.value0));if(a instanceof Jt)return U(e.Bind1())(n)(function(u){if(u instanceof jt)return h(e.Applicative0())(new jt(u.value0));if(u instanceof Jt)return h(e.Applicative0())(new Jt(gt(t)(a.value0)(u.value0)));throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): "+[u.constructor.name])});throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): "+[a.constructor.name])})}},Functor0:function(){return __(e.Bind1().Apply0().Functor0())}}}};var nD=function(){var t=Ar();return function(e){return t(us(e))}}();function Rg(t,e,r){return t==null?e:r(t)}var Xr=function(t){return Rg(t,z.value,N.create)};function s_(t){return Object.prototype.toString.call(t).slice(8,-1)}var qg=Array.isArray||function(t){return Object.prototype.toString.call(t)==="[object Array]"};var fD=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}();var Hg=ct;var cD=function(t){var e=ro(Og(t));return function(r){return e(Jv(r))}};var lD=function(t){return function(e){return function(r){if(s_(r)===e)return h(p_(t))(Hg(r));if(tr)return cD(t)(new fD(e,s_(r)));throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): "+[e.constructor.name,r.constructor.name])}}};var _D=function(t){return lD(t)("String")};var cs=function(){function t(){}return t.value=new t,t}(),ls=function(){function t(){}return t.value=new t,t}(),Jg=function(){function t(){}return t.value=new t,t}(),jg=function(){function t(){}return t.value=new t,t}(),sD=function(){function t(){}return t.value=new t,t}(),Xg=function(){function t(){}return t.value=new t,t}(),Qg=function(){function t(){}return t.value=new t,t}();var Kg=function(t){return t},Yg=function(t){return t};var Zg=function(t){return t};var tC=function(t){return t};var eC=function(t){return t};var rC=function(t){return t},nC=function(t){return t},aC=function(t){return t},uC=function(t){return t},oC=function(t){return t};var mD=function(){function t(){}return t.value=new t,t}(),iC=function(){function t(){}return t.value=new t,t}(),fC=function(){function t(){}return t.value=new t,t}(),vD=function(){function t(){}return t.value=new t,t}(),cC=function(){function t(){}return t.value=new t,t}();var _s=function(t){return t};var gc=function(t){return t};var eP=function(t){return t},m_=function(t){return t};var wf={toAudioOnOff:Z(tt)};var Pf=function(t){return t.toAudioParameter},lC=function(t){return t.toAudioOnOff},_C=function(){return Bl.create}(),pC=function(){return Gl.value}();var ps=function(){return Mg(function(){var t=xg()()(ga),e=$g({reflectSymbol:function(){return"o"}})()()(d.value)(Dn);return function(r){return t(e(r))}}())},sC=ct;var rP=function(){var t=_r()({reflectSymbol:function(){return"unit"}})(d.value);return function(e){return m_(t(e))}}();var nP=function(t){return function(e){return{toAudioParameter:function(r){return rP(r)}}}},mC=function(t){return function(e){return{toAudioParameter:function(){var r=Pf(nP(t)(e));return function(n){return r(eP(function(a){return{u:a}}(n)))}}()}}},vC=function(){return _r()({reflectSymbol:function(){return"2x"}})(d.value)(void 0)}(),DC=function(){var t=_r()({reflectSymbol:function(){return"sudden"}})(d.value);return function(e){return m_(t(e))}}();var dC={toAudioParameter:DC},ss={toAudioParameter:function(t){return DC({n:t})}},DD=function(){return _r()({reflectSymbol:function(){return"step"}})(d.value)(void 0)}();var dD=function(){return _r()({reflectSymbol:function(){return"on"}})(d.value)(void 0)}(),v_={x:dD,o:0},lt=function(){return K(S(o))(en()(_r()({reflectSymbol:function(){return"onOff"}})(d.value)(v_)))};var bC=function(){return _r()({reflectSymbol:function(){return"off"}})(d.value)(void 0)}();var aP=function(){var t=_r()({reflectSymbol:function(){return"numeric"}})(d.value);return function(e){return m_(t(e))}}();var Ir={toAudioParameter:aP};var Wo=function(){return _r()({reflectSymbol:function(){return"linear"}})(d.value)(void 0)}();var AC=function(){return _r()({reflectSymbol:function(){return"exponential"}})(d.value)(void 0)}(),uP=function(){var t=_r()({reflectSymbol:function(){return"envelope"}})(d.value);return function(e){return m_(t(e))}}();var Tn={toAudioParameter:uP},oP=function(){var t=_r()({reflectSymbol:function(){return"cancel"}})(d.value);return function(e){return m_(t(e))}}();var yC={toAudioParameter:oP};var iP=function(){function t(){}return t.value=new t,t}(),fP=function(){function t(){}return t.value=new t,t}(),cP=function(){function t(){}return t.value=new t,t}(),lP=function(){function t(){}return t.value=new t,t}(),_P=function(){function t(){}return t.value=new t,t}(),pP=function(){function t(){}return t.value=new t,t}(),sP=function(){function t(){}return t.value=new t,t}(),mP=function(){function t(){}return t.value=new t,t}(),vP=function(){function t(){}return t.value=new t,t}(),DP=function(){function t(){}return t.value=new t,t}(),dP=function(){function t(){}return t.value=new t,t}(),bP=function(){function t(){}return t.value=new t,t}(),AP=function(){function t(){}return t.value=new t,t}(),yP=function(){function t(){}return t.value=new t,t}(),si=function(t){return{toPeriodicOscSpec:function(e){return _r()({reflectSymbol:function(){return"realImg"}})(d.value)({real:es(e.value0),img:es(e.value1)})}}};var ms={toInitializeTriangleOsc:function(t){return oC(function(e){return{frequency:e}}(t))}};var kC={toInitializeStereoPanner:function(t){return uC(function(e){return{pan:e}}(t))}};var Cc={toInitializeSquareOsc:function(t){return aC(function(e){return{frequency:e}}(t))}};var Yi={toInitializeSinOsc:function(t){return nC(function(e){return{frequency:e}}(t))}};var gC={toInitializeSawtoothOsc:function(t){return rC(function(e){return{frequency:e}}(t))}};var bD={toInitializeRecorder:function(t){return Kg(function(e){return{cb:e}}(t))}};var D_={toInitializeMicrophone:function(t){return Yg(function(e){return{microphone:e}}(t))}};var CC=function(t){return function(e){return{toInitializeIIRFilter:function(r){return function(n){return function(a){return{feedforward:Xv(pi)(Fu()(r.value0)),feedback:Xv(pi)(Fu()(r.value1))}}}}}}};var ot={toInitializeGain:function(t){return eC(function(e){return{gain:e}}(t))}};var EC={toInitializeConvolver:function(t){return Zg(function(e){return{buffer:e}}(t))}},vs={toInitializeConstant:function(t){return tC(function(e){return{offset:e}}(t))}};var kP={convertOption:function(t){return function(e){return Z(tt)}}},d_={convertOption:function(t){return function(e){return Z(tt)}}},SC={convertOption:function(t){return function(e){return Z(tt)}}},hC={convertOption:function(t){return function(e){return N.create}}},TC={convertOption:function(t){return function(e){return Z(tt)}}},mi={convertOption:function(t){return function(e){return Z(tt)}}},Ec={convertOption:function(t){return function(e){return Z(tt)}}},Sc={convertOption:function(t){return function(e){return Z(tt)}}},hc={convertOption:function(t){return function(e){return Z(tt)}}},Tc={convertOption:function(t){return function(e){return Z(tt)}}},xc={convertOption:function(t){return function(e){return Z(tt)}}},xC={convertOption:function(t){return function(e){return Z(tt)}}},FC={convertOption:function(t){return function(e){return Z(tt)}}},$C={convertOption:function(t){return function(e){return Z(tt)}}},AD={convertOption:function(t){return function(e){return Z(tt)}}},Of={convertOption:function(t){return function(e){return Z(tt)}}},b_={convertOption:function(t){return function(e){return Z(tt)}}},A_={convertOption:function(t){return function(e){return Z(tt)}}};var Fc={convertOption:function(t){return function(e){return Z(tt)}}},MC={convertOption:function(t){return function(e){return Z(tt)}}},wC={convertOption:function(t){return function(e){return Z(tt)}}},PC={convertOption:function(t){return function(e){return Z(tt)}}},yD={convertOption:function(t){return function(e){return Z(tt)}}};var OC={convertOption:function(t){return function(e){return Z(tt)}}},kD={convertOption:function(t){return function(e){return Z(tt)}}},bn={convertOption:function(t){return function(e){return Z(tt)}}},un={convertOption:function(t){return function(e){return Z(tt)}}},gD={convertOption:function(t){return function(e){return Z(tt)}}},Ds={convertOption:function(t){return function(e){return Z(tt)}}},gP=function(t){return t.toPeriodicOscSpec},vi=function(t){return{convertOption:function(e){return function(r){return gP(t)}}}},CD=function(t){return t.toInitializeWaveShaper},IC=function(t){return t.toInitializeTriangleOsc},RC=function(t){return t.toInitializeStereoPanner},NC=function(t){return t.toInitializeSquareOsc},LC=function(t){return t.toInitializeSinOsc},WC=function(t){return t.toInitializeSawtoothOsc},BC=function(t){return t.toInitializeRecorder},ED=function(t){return t.toInitializePlayBuf},GC=function(t){return t.toInitializePeriodicOsc},UC=function(t){return t.toInitializePeaking},qC=function(t){return t.toInitializeNotch},HC=function(t){return t.toInitializeMicrophone},zC=function(t){return t.toInitializeLowshelf},SD=function(t){return t.toInitializeLowpass},hD=function(t){return t.toInitializeLoopBuf},VC=function(t){return t.toInitializeIIRFilter},JC=function(t){return t.toInitializeHighshelf},TD=function(t){return t.toInitializeHighpass},jC=function(t){return t.toInitializeGain},XC=function(t){return t.toInitializeDynamicsCompressor},xD=function(t){return t.toInitializeDelay},QC=function(t){return t.toInitializeConvolver},KC=function(t){return t.toInitializeConstant},FD=function(t){return t.toInitializeBandpass},$D=function(t){return t.toInitializeAllpass};var CP={oversample:vC},EP=function(t){return{toInitializeWaveShaper:function(e){return oa(t)(iP.value)(CP)(e)}}},YC={toInitializeWaveShaper:function(){var t=CD(EP(kt(yt()(J(At)(kP)()()()({reflectSymbol:function(){return"curve"}})))(bt()())));return function(e){return t(function(r){return{curve:r}}(e))}}()},SP=function(){return{bufferOffset:0,playbackRate:1,duration:z.value}}(),y_=function(t){return{toInitializePlayBuf:function(e){return oa(t)(fP.value)(SP)(e)}}},Ba={toInitializePlayBuf:function(){var t=ED(y_(kt(yt()(J(At)(d_)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())));return function(e){return t(function(r){return{buffer:r}}(e))}}()},hP={},Di=function(t){return{toInitializePeriodicOsc:function(e){return oa(t)(cP.value)(hP)(e)}}},TP={q:1,gain:0},$c=function(t){return{toInitializePeaking:function(e){return oa(t)(lP.value)(TP)(e)}}};var xP={q:1},Mc=function(t){return{toInitializeNotch:function(e){return oa(t)(_P.value)(xP)(e)}}};var FP={gain:0},ZC=function(t){return{toInitializeLowshelf:function(e){return oa(t)(pP.value)(FP)(e)}}};var $P={q:1},MD=function(t){return{toInitializeLowpass:function(e){return oa(t)(sP.value)($P)(e)}}},ds={toInitializeLowpass:function(){var t=SD(MD(kt(yt()(J(At)(AD)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()},MP=function(){return{loopStart:0,loopEnd:0,playbackRate:1,duration:z.value}}(),If=function(t){return{toInitializeLoopBuf:function(e){return oa(t)(mP.value)(MP)(e)}}},ge={toInitializeLoopBuf:function(){var t=hD(If(kt(yt()(J(At)(Of)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())));return function(e){return t(function(r){return{buffer:r}}(e))}}()},wP={gain:0},tE=function(t){return{toInitializeHighshelf:function(e){return oa(t)(vP.value)(wP)(e)}}};var PP={q:1},wD=function(t){return{toInitializeHighpass:function(e){return oa(t)(DP.value)(PP)(e)}}},Ja={toInitializeHighpass:function(){var t=TD(wD(kt(yt()(J(At)(yD)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()},OP=function(){return{ratio:12,attack:.003,release:.25,knee:30,threshold:-24}}(),eE=function(t){return{toInitializeDynamicsCompressor:function(e){return oa(t)(dP.value)(OP)(e)}}},IP={maxDelayTime:1},PD=function(t){return{toInitializeDelay:function(e){return oa(t)(bP.value)(IP)(e)}}},Qr={toInitializeDelay:function(){var t=xD(PD(kt(yt()(J(At)(kD)()()()({reflectSymbol:function(){return"delayTime"}})))(bt()())));return function(e){return t(function(r){return{delayTime:r}}(e))}}()},RP={q:1},on=function(t){return{toInitializeBandpass:function(e){return oa(t)(AP.value)(RP)(e)}}},OD={toInitializeBandpass:function(){var t=FD(on(kt(yt()(J(At)(un)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()},NP={q:1},bs=function(t){return{toInitializeAllpass:function(e){return oa(t)(yP.value)(NP)(e)}}},ID={toInitializeAllpass:function(){var t=$D(bs(kt(yt()(J(At)(Ds)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()};var Bo=function(){function t(){this.head=null,this.last=null,this.size=0}function e(_,v){this.queue=_,this.value=v,this.next=null,this.prev=null}function r(_){this.draining=!1,this.error=null,this.value=_,this.takes=new t,this.reads=new t,this.puts=new t}var n={};function a(_){try{_()}catch(v){setTimeout(function(){throw v},0)}}function u(_,v){var D=new e(_,v);switch(_.size){case 0:_.head=D;break;case 1:D.prev=_.head,_.head.next=D,_.last=D;break;default:D.prev=_.last,_.last.next=D,_.last=D}return _.size++,D}function f(_){var v;switch(_.size){case 0:return null;case 1:v=_.head,_.head=null;break;case 2:v=_.last,_.head.next=null,_.last=null;break;default:v=_.last,_.last=v.prev,_.last.next=null}return v.prev=null,v.queue=null,_.size--,v.value}function i(_){var v;switch(_.size){case 0:return null;case 1:v=_.head,_.head=null;break;case 2:v=_.head,_.last.prev=null,_.head=_.last,_.last=null;break;default:v=_.head,_.head=v.next,_.head.prev=null}return v.next=null,v.queue=null,_.size--,v.value}function m(_){if(_.queue!==null){if(_.queue.last===_){f(_.queue);return}if(_.queue.head===_){i(_.queue);return}_.prev&&(_.prev.next=_.next),_.next&&(_.next.prev=_.prev),_.queue.size--,_.queue=null,_.value=null,_.next=null,_.prev=null}}function s(_,v){if(!v.draining){var D=v.puts,l=v.takes,g=v.reads,nt,st,me,Qt,Je;for(v.draining=!0;;){if(nt=null,st=null,me=null,Qt=v.value,Je=g.size,v.error!==null){for(Qt=_.left(v.error);nt=i(D);)a(nt.cb(Qt));for(;st=i(g);)a(st(Qt));for(;me=i(l);)a(me(Qt));break}if(Qt===n&&(nt=i(D))&&(v.value=Qt=nt.value),Qt!==n){for(me=i(l);Je--&&(st=i(g));)a(st(_.right(Qt)));me!==null&&(v.value=n,a(me(_.right(Qt))))}if(nt!==null&&a(nt.cb(_.right(void 0))),v.value===n&&D.size===0||v.value!==n&&l.size===0)break}v.draining=!1}}return r.EMPTY=n,r.putLast=u,r.takeLast=f,r.takeHead=i,r.deleteCell=m,r.drainVar=s,r}();function k_(){return new Bo(Bo.EMPTY)}function rE(t,e,r){return function(){var n=Bo.putLast(e.takes,r);return Bo.drainVar(t,e),function(){Bo.deleteCell(n)}}}function nE(t,e,r){return function(){return r.value===Bo.EMPTY&&r.error===null?(r.value=e,Bo.drainVar(t,r),!0):!1}}function aE(t,e){return function(){var r=e.value;return r===Bo.EMPTY?t.nothing:(e.value=Bo.EMPTY,Bo.drainVar(t,e),t.just(r))}}var GP=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),UP=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),qP=function(){function t(){}return t.value=new t,t}();var RD=function(){return{left:Jt.create,right:jt.create,nothing:z.value,just:N.create,killed:GP.create,filled:UP.create,empty:qP.value}}();var uE=function(t){return function(e){return rE(RD,t,e)}},As=function(t){return function(e){return nE(RD,t,e)}};var oE=function(t){return aE(RD,t)};var fE=function(t,e,r){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return n=1,a=r(),n=2,a}};var HP=function(){function t(){}return t.value=new t,t}();var ys={convertOption:function(t){return function(e){return Z(tt)}}},ks={convertOption:function(t){return function(e){return Z(tt)}}};var zP=function(t){return t.toInitializeAnalyser};var VP=function(){return{cb:function(t){return h(c)(h(c)(void 0))},fftSize:sD.value,maxDecibels:-30,minDecibels:-100,smoothingTimeConstant:.8,channelCount:2,channelCountMode:vD.value,channelInterpretation:mD.value}}(),gs=function(t){return{toInitializeAnalyser:function(e){return oa(t)(HP.value)(VP)(e)}}};var JP=function(t){return function(e){var r=HC(t)(e),n=function(a){return function(u){return Wt(function(f){return function(){var m=u.ids();return a.raiseId(m)(),p(x)(function(s){return j(rt)(f(u.deleteFromCache({id:m})))(s)})(Tt(xt)(f)(wt(c)(u.makeMicrophone({id:m,parent:a.parent,scope:a.scope,microphone:r.microphone}))))()}})}};return new w(n)}},g_=function(t){return JP(t)};var Pt=function(t){return function(e){return function(r){return Lr(t)(e)(M(C(c)))(r)}}},Lr=function(t){return function(e){return function(r){return function(n){var a=jC(t)(e),u=function(f){return function(i){return Wt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(rt)(m(i.deleteFromCache({id:_})))(v)})(Tt(xt)(m)(I(O(c))(wt(c)(i.makeGain({id:_,parent:f.parent,scope:f.scope,gain:a.gain})))(I(O(c))(hr(S(o))(p(k)(function(v){return Nr()()()({gain:cE(591)(f.scope)(i)(function(D){return i.setGain(function(l){return{id:_,gain:l}}(D))})})(v)})(r)))(ND(597)({parent:new N(_),scope:f.scope,raiseId:Ot(Hr(zr(Zr)))})(i)(B(n))))))()}})}};return new w(u)}}}},ND=fE("__internalWagsFlatten","WAGS.Control",function(){return to(c)(o)({doLogic:pf,ids:function(){var t=Ar();return function(e){return function(r){return r.ids}(t(e))}}(),disconnectElement:function(t){return function(e){return t.disconnectXFromY({from:e.id,to:e.parent})}},wrapElt:function(t){return Pt(ot)(1)([t])},toElt:function(t){return t}})}),cE=fE("tmpResolveAU","WAGS.Control",function(){var t=function(){var f=_r()({reflectSymbol:function(){return"unit"}})(d.value);return function(i){return gc(f(i))}}(),e=function(){var f=_r()({reflectSymbol:function(){return"sudden"}})(d.value);return function(i){return gc(f(i))}}(),r=function(){var f=_r()({reflectSymbol:function(){return"numeric"}})(d.value);return function(i){return gc(f(i))}}(),n=function(){var f=_r()({reflectSymbol:function(){return"envelope"}})(d.value);return function(i){return gc(f(i))}}(),a=function(){var f=_r()({reflectSymbol:function(){return"cancel"}})(d.value);return function(i){return gc(f(i))}}(),u=function(f){return function(i){return function(m){return function(s){return Nr()()()({numeric:function(){var _=wt(c);return function(v){return _(m(r(v)))}}(),envelope:function(){var _=wt(c);return function(v){return _(m(n(v)))}}(),cancel:function(){var _=wt(c);return function(v){return _(m(a(v)))}}(),sudden:function(){var _=wt(c);return function(v){return _(m(e(v)))}}(),unit:function(_){var v=Pt(ot)(1)([_.u]);return Wt(function(D){return function(){var g=k_();return xt(I(O(c))(ND(1639)({parent:z.value,scope:f,raiseId:function(nt){return Ae(x)(As(nt)(g))}})(i)(v))(Wt(function(nt){return function(){return Ae(x)(uE(g)(function(me){if(me instanceof Jt)return hf(me.value0);if(me instanceof jt)return nt(m(t({i:me.value0})));throw new Error("Failed pattern match at WAGS.Control (line 1640, column 39 - line 1643, column 66): "+[me.constructor.name])}))(),h(c)(void 0)}})))(D)()}})}})(s)}}}};return u}),pa=ND(1650),or=cE(1619),jP=function(t){return function(e){return function(r){return function(n){var a=zP(t)(e),u=function(f){return function(i){return Wt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(rt)(m(i.deleteFromCache({id:_})))(v)})(Tt(xt)(m)(I(O(c))(wt(c)(i.makeAnalyser({id:_,parent:f.parent,scope:f.scope,cb:a.cb,fftSize:gv(2)(function(){if(a.fftSize instanceof cs)return 7;if(a.fftSize instanceof ls)return 8;if(a.fftSize instanceof Jg)return 9;if(a.fftSize instanceof jg)return 10;if(a.fftSize instanceof sD)return 11;if(a.fftSize instanceof Xg)return 12;if(a.fftSize instanceof Qg)return 13;throw new Error("Failed pattern match at WAGS.Control (line 189, column 21 - line 196, column 34): "+[a.fftSize.constructor.name])}()),maxDecibels:a.maxDecibels,minDecibels:a.minDecibels,smoothingTimeConstant:a.smoothingTimeConstant,channelCount:a.channelCount,channelCountMode:function(){if(a.channelCountMode instanceof cC)return"explicit";if(a.channelCountMode instanceof vD)return"max";if(a.channelCountMode instanceof fC)return"clamped-max";throw new Error("Failed pattern match at WAGS.Control (line 202, column 35 - line 205, column 46): "+[a.channelCountMode.constructor.name])}(),channelInterpretation:function(){if(a.channelInterpretation instanceof mD)return"speakers";if(a.channelInterpretation instanceof iC)return"discrete";throw new Error("Failed pattern match at WAGS.Control (line 206, column 40 - line 208, column 41): "+[a.channelInterpretation.constructor.name])}()})))(I(O(c))(p(k)(function(v){return Nr()()()({cb:function(D){return i.setAnalyserNodeCb({id:_,cb:D})}})(v)})(r))(pa({parent:new N(_),scope:f.scope,raiseId:Ot(Hr(zr(Zr)))})(i)(B(n))))))()}})}};return new w(u)}}}},Cs=function(t){return function(e){return jP(t)(e)(M(C(c)))}},lE=function(t){return function(e){return function(r){var n=QC(t)(e),a=function(u){return function(f){return Wt(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(rt)(i(f.deleteFromCache({id:s})))(_)})(Tt(xt)(i)(I(O(c))(wt(c)(f.makeConvolver({id:s,parent:u.parent,scope:u.scope,buffer:n.buffer})))(pa({parent:new N(s),scope:u.scope,raiseId:Ot(Hr(zr(Zr)))})(f)(B(r)))))()}})}};return new w(a)}}},XP=function(){return function(){return function(t){return function(e){return function(r){return function(n){return function(a){var u=VC(t)(n)(e)(r),f=function(i){return function(m){return Wt(function(s){return function(){var v=m.ids();return i.raiseId(v)(),p(x)(function(D){return j(rt)(s(m.deleteFromCache({id:v})))(D)})(Tt(xt)(s)(I(O(c))(wt(c)(m.makeIIRFilter({id:v,parent:i.parent,scope:i.scope,feedforward:Cf()(u.feedforward),feedback:Cf()(u.feedback)})))(pa({parent:new N(v),scope:i.scope,raiseId:Ot(Hr(zr(Zr)))})(m)(B(a)))))()}})}};return new w(f)}}}}}}},_E=function(){return function(){return function(t){return XP()()(t)(d.value)(d.value)}}},LD=function(t){return function(e){return function(r){var n=BC(t)(e),a=function(u){return function(f){return Wt(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(rt)(i(f.deleteFromCache({id:s})))(_)})(Tt(xt)(i)(I(O(c))(wt(c)(f.makeRecorder({id:s,parent:u.parent,scope:u.scope,cb:n.cb})))(pa({parent:new N(s),scope:u.scope,raiseId:Ot(Hr(zr(Zr)))})(f)(r))))()}})}};return new w(a)}}},QP=function(t){return function(e){return Wt(function(r){return function(){var a=e.ids();return r(e.makeSpeaker({id:a}))(),xt(pa({parent:new N(a),scope:new Ji("toplevel"),raiseId:Ot(Hr(zr(Zr)))})(e)(B(t)))(r)()}})}},Rf=QP,KP=function(t){return function(e){return function(r){var n=hD(t)(e),a=function(u){return function(f){return Wt(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(rt)(i(f.deleteFromCache({id:s})))(_)})(Tt(xt)(i)(I(O(c))(wt(c)(f.makeLoopBuf({id:s,parent:u.parent,scope:u.scope,buffer:n.buffer,playbackRate:n.playbackRate,loopStart:n.loopStart,loopEnd:n.loopEnd,duration:n.duration})))(hr(S(o))(p(k)(function(_){return Nr()()()({buffer:function(v){return wt(c)(f.setBuffer({id:s,buffer:v}))},playbackRate:or(u.scope)(f)(function(v){return f.setPlaybackRate(function(D){return{id:s,playbackRate:D}}(v))}),loopStart:function(v){return wt(c)(f.setLoopStart({id:s,loopStart:v}))},loopEnd:function(v){return wt(c)(f.setLoopEnd({id:s,loopEnd:v}))},onOff:function(v){return wt(c)(f.setOnOff({id:s,onOff:v}))}})(_)})(r)))))()}})}};return new w(a)}}},ie=function(t){return KP(t)};var YP=function(t){return function(e){return function(r){var n=GC(t)(e),a=function(u){return function(f){return Wt(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(rt)(i(f.deleteFromCache({id:s})))(_)})(Tt(xt)(i)(I(O(c))(wt(c)(f.makePeriodicOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency,spec:n.spec})))(hr(S(o))(p(k)(function(_){return Nr()()()({frequency:or(u.scope)(f)(function(v){return f.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return wt(c)(f.setOnOff({id:s,onOff:v}))},spec:function(v){return wt(c)(f.setPeriodicOsc({id:s,spec:v}))}})(_)})(r)))))()}})}};return new w(a)}}},di=function(t){return YP(t)};var ZP=function(t){return function(e){return function(r){var n=ED(t)(e),a=function(u){return function(f){return Wt(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(rt)(i(f.deleteFromCache({id:s})))(_)})(Tt(xt)(i)(I(O(c))(wt(c)(f.makePlayBuf({id:s,parent:u.parent,scope:u.scope,buffer:n.buffer,playbackRate:n.playbackRate,bufferOffset:n.bufferOffset,duration:n.duration})))(hr(S(o))(p(k)(function(_){return Nr()()()({buffer:function(v){return wt(c)(f.setBuffer({id:s,buffer:v}))},playbackRate:or(u.scope)(f)(function(v){return f.setPlaybackRate(function(D){return{id:s,playbackRate:D}}(v))}),bufferOffset:function(v){return wt(c)(f.setBufferOffset({id:s,bufferOffset:v}))},onOff:function(v){return wt(c)(f.setOnOff({id:s,onOff:v}))},duration:function(v){return wt(c)(f.setDuration({id:s,duration:v}))}})(_)})(r)))))()}})}};return new w(a)}}},Gn=function(t){return ZP(t)};var tO=function(t){return function(e){return function(r){var n=WC(t)(e),a=function(u){return function(f){return Wt(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(rt)(i(f.deleteFromCache({id:s})))(_)})(Tt(xt)(i)(I(O(c))(wt(c)(f.makeSawtoothOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(hr(S(o))(p(k)(function(_){return Nr()()()({frequency:or(u.scope)(f)(function(v){return f.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return wt(c)(f.setOnOff({id:s,onOff:v}))}})(_)})(r)))))()}})}};return new w(a)}}},pE=function(t){return tO(t)};var eO=function(t){return function(e){return function(r){var n=LC(t)(e),a=function(u){return function(f){return Wt(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(rt)(i(f.deleteFromCache({id:s})))(_)})(Tt(xt)(i)(I(O(c))(wt(c)(f.makeSinOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(hr(S(o))(p(k)(function(_){return Nr()()()({frequency:or(u.scope)(f)(function(v){return f.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return wt(c)(f.setOnOff({id:s,onOff:v}))}})(_)})(r)))))()}})}};return new w(a)}}},Nf=function(t){return eO(t)},sE=function(t){return function(e){return Nf(t)(e)(M(C(c)))}},rO=function(t){return function(e){return function(r){var n=NC(t)(e),a=function(u){return function(f){return Wt(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(rt)(i(f.deleteFromCache({id:s})))(_)})(Tt(xt)(i)(I(O(c))(wt(c)(f.makeSquareOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(hr(S(o))(p(k)(function(_){return Nr()()()({frequency:or(u.scope)(f)(function(v){return f.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return wt(c)(f.setOnOff({id:s,onOff:v}))}})(_)})(r)))))()}})}};return new w(a)}}},C_=function(t){return rO(t)},mE=function(t){return function(e){return C_(t)(e)(M(C(c)))}},nO=function(t){return function(e){return function(r){var n=IC(t)(e),a=function(u){return function(f){return Wt(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(rt)(i(f.deleteFromCache({id:s})))(_)})(Tt(xt)(i)(I(O(c))(wt(c)(f.makeTriangleOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(hr(S(o))(p(k)(function(_){return Nr()()()({frequency:or(u.scope)(f)(function(v){return f.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return wt(c)(f.setOnOff({id:s,onOff:v}))}})(_)})(r)))))()}})}};return new w(a)}}},Es=function(t){return nO(t)};var aO=function(t){return function(e){return function(r){return function(n){var a=$D(t)(e),u=function(f){return function(i){return Wt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(rt)(m(i.deleteFromCache({id:_})))(v)})(Tt(xt)(m)(I(O(c))(wt(c)(i.makeAllpass({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,q:a.q})))(I(O(c))(hr(S(o))(p(k)(function(v){return Nr()()()({frequency:or(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),q:or(f.scope)(i)(function(D){return i.setQ(function(l){return{id:_,q:l}}(D))})})(v)})(r)))(pa({parent:new N(_),scope:f.scope,raiseId:Ot(Hr(zr(Zr)))})(i)(B(n))))))()}})}};return new w(u)}}}},E_=function(t){return function(e){return function(r){return aO(t)(e)(M(C(c)))(r)}}},WD=function(t){return function(e){return function(r){return function(n){var a=FD(t)(e),u=function(f){return function(i){return Wt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(rt)(m(i.deleteFromCache({id:_})))(v)})(Tt(xt)(m)(I(O(c))(wt(c)(i.makeBandpass({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,q:a.q})))(I(O(c))(hr(S(o))(p(k)(function(v){return Nr()()()({frequency:or(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),q:or(f.scope)(i)(function(D){return i.setQ(function(l){return{id:_,q:l}}(D))})})(v)})(r)))(pa({parent:new N(_),scope:f.scope,raiseId:Ot(Hr(zr(Zr)))})(i)(B(n))))))()}})}};return new w(u)}}}},An=function(t){return function(e){return function(r){return WD(t)(e)(M(C(c)))(r)}}},S_=function(t){return function(e){return function(r){return function(n){var a=xD(t)(e),u=function(f){return function(i){return Wt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(rt)(m(i.deleteFromCache({id:_})))(v)})(Tt(xt)(m)(I(O(c))(wt(c)(i.makeDelay({id:_,parent:f.parent,scope:f.scope,delayTime:a.delayTime,maxDelayTime:a.maxDelayTime})))(I(O(c))(hr(S(o))(p(k)(function(v){return Nr()()()({delayTime:or(f.scope)(i)(function(D){return i.setDelay(function(l){return{id:_,delayTime:l}}(D))})})(v)})(r)))(pa({parent:new N(_),scope:f.scope,raiseId:Ot(Hr(zr(Zr)))})(i)(B(n))))))()}})}};return new w(u)}}}},po=function(t){return function(e){return function(r){return S_(t)(e)(M(C(c)))(r)}}},uO=function(t){return function(e){return function(r){return function(n){var a=XC(t)(e),u=function(f){return function(i){return Wt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(rt)(m(i.deleteFromCache({id:_})))(v)})(Tt(xt)(m)(I(O(c))(wt(c)(i.makeDynamicsCompressor({id:_,parent:f.parent,scope:f.scope,threshold:a.threshold,ratio:a.ratio,knee:a.knee,attack:a.attack,release:a.release})))(I(O(c))(hr(S(o))(p(k)(function(v){return Nr()()()({threshold:or(f.scope)(i)(function(D){return i.setThreshold(function(l){return{id:_,threshold:l}}(D))}),ratio:or(f.scope)(i)(function(D){return i.setRatio(function(l){return{id:_,ratio:l}}(D))}),knee:or(f.scope)(i)(function(D){return i.setKnee(function(l){return{id:_,knee:l}}(D))}),attack:or(f.scope)(i)(function(D){return i.setAttack(function(l){return{id:_,attack:l}}(D))}),release:or(f.scope)(i)(function(D){return i.setRelease(function(l){return{id:_,release:l}}(D))})})(v)})(r)))(pa({parent:new N(_),scope:f.scope,raiseId:Ot(Hr(zr(Zr)))})(i)(B(n))))))()}})}};return new w(u)}}}},vE=function(t){return function(e){return uO(t)(e)(M(C(c)))}},oO=function(){return function(t){return function(e){return Dv()(o)({doLogic:pf,ids:function(){var r=Ar();return function(n){return function(a){return a.ids}(r(n))}}(),disconnectElement:function(r){return function(n){return r.disconnectXFromY({from:n.id,to:n.parent})}},wrapElt:function(r){return Pt(ot)(1)([r])},toElt:function(r){return r}})({fromElt:function(r){return r},giveNewParent:function(r){return function(n){return r.connectXToY({from:n.id,to:n.parent})}},deleteFromCache:function(){var r=Ar();return function(n){return function(a){return a.deleteFromCache}(r(n))}}()})(t)(e)}}},Sa=function(t){return function(e){return oO()(jy(t))(Yu(ga)(Xy()()()()()({reflectType:function(){return 0}})(d.value))(e))}},ja=ok(o)({doLogic:pf,ids:function(){var t=Ar();return function(e){return function(r){return r.ids}(t(e))}}(),disconnectElement:function(t){return function(e){return t.disconnectXFromY({from:e.id,to:e.parent})}},wrapElt:function(t){return Pt(ot)(1)([t])},toElt:function(t){return t}})({fromElt:function(t){return t},connectToParent:function(t){return function(e){return t.connectXToY({from:e.id,to:e.parent})}}});var BD=function(t){return function(e){return function(r){return function(n){var a=TD(t)(e),u=function(f){return function(i){return Wt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(rt)(m(i.deleteFromCache({id:_})))(v)})(Tt(xt)(m)(I(O(c))(wt(c)(i.makeHighpass({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,q:a.q})))(I(O(c))(hr(S(o))(p(k)(function(v){return Nr()()()({frequency:or(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),q:or(f.scope)(i)(function(D){return i.setQ(function(l){return{id:_,q:l}}(D))})})(v)})(r)))(pa({parent:new N(_),scope:f.scope,raiseId:Ot(Hr(zr(Zr)))})(i)(B(n))))))()}})}};return new w(u)}}}},wc=function(t){return function(e){return function(r){return BD(t)(e)(M(C(c)))(r)}}},iO=function(t){return function(e){return function(r){return function(n){var a=JC(t)(e),u=function(f){return function(i){return Wt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(rt)(m(i.deleteFromCache({id:_})))(v)})(Tt(xt)(m)(I(O(c))(wt(c)(i.makeHighshelf({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,gain:a.gain})))(I(O(c))(hr(S(o))(p(k)(function(v){return Nr()()()({frequency:or(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),gain:or(f.scope)(i)(function(D){return i.setGain(function(l){return{id:_,gain:l}}(D))})})(v)})(r)))(pa({parent:new N(_),scope:f.scope,raiseId:Ot(Hr(zr(Zr)))})(i)(B(n))))))()}})}};return new w(u)}}}},DE=function(t){return function(e){return function(r){return iO(t)(e)(M(C(c)))(r)}}},dE=function(t){return function(e){return function(r){return function(n){var a=SD(t)(e),u=function(f){return function(i){return Wt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(rt)(m(i.deleteFromCache({id:_})))(v)})(Tt(xt)(m)(I(O(c))(wt(c)(i.makeLowpass({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,q:a.q})))(I(O(c))(hr(S(o))(p(k)(function(v){return Nr()()()({frequency:or(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),q:or(f.scope)(i)(function(D){return i.setQ(function(l){return{id:_,q:l}}(D))})})(v)})(r)))(pa({parent:new N(_),scope:f.scope,raiseId:Ot(Hr(zr(Zr)))})(i)(B(n))))))()}})}};return new w(u)}}}},Pc=function(t){return function(e){return function(r){return dE(t)(e)(M(C(c)))(r)}}},fO=function(t){return function(e){return function(r){return function(n){var a=zC(t)(e),u=function(f){return function(i){return Wt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(rt)(m(i.deleteFromCache({id:_})))(v)})(Tt(xt)(m)(I(O(c))(wt(c)(i.makeLowshelf({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,gain:a.gain})))(I(O(c))(hr(S(o))(p(k)(function(v){return Nr()()()({frequency:or(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),gain:or(f.scope)(i)(function(D){return i.setGain(function(l){return{id:_,gain:l}}(D))})})(v)})(r)))(pa({parent:new N(_),scope:f.scope,raiseId:Ot(Hr(zr(Zr)))})(i)(B(n))))))()}})}};return new w(u)}}}},bE=function(t){return function(e){return function(r){return fO(t)(e)(M(C(c)))(r)}}},cO=function(t){return function(e){return function(r){return function(n){var a=qC(t)(e),u=function(f){return function(i){return Wt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(rt)(m(i.deleteFromCache({id:_})))(v)})(Tt(xt)(m)(I(O(c))(wt(c)(i.makeNotch({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,q:a.q})))(I(O(c))(hr(S(o))(p(k)(function(v){return Nr()()()({frequency:or(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),q:or(f.scope)(i)(function(D){return i.setQ(function(l){return{id:_,q:l}}(D))})})(v)})(r)))(pa({parent:new N(_),scope:f.scope,raiseId:Ot(Hr(zr(Zr)))})(i)(B(n))))))()}})}};return new w(u)}}}},Oc=function(t){return function(e){return function(r){return cO(t)(e)(M(C(c)))(r)}}},lO=function(t){return function(e){return function(r){return function(n){var a=RC(t)(e),u=function(f){return function(i){return Wt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(rt)(m(i.deleteFromCache({id:_})))(v)})(Tt(xt)(m)(I(O(c))(wt(c)(i.makeStereoPanner({id:_,parent:f.parent,scope:f.scope,pan:a.pan})))(I(O(c))(hr(S(o))(p(k)(function(v){return Nr()()()({pan:or(f.scope)(i)(function(D){return i.setPan(function(l){return{id:_,pan:l}}(D))})})(v)})(r)))(pa({parent:new N(_),scope:f.scope,raiseId:Ot(Hr(zr(Zr)))})(i)(B(n))))))()}})}};return new w(u)}}}},AE=function(t){return function(e){return lO(t)(e)(M(C(c)))}},_O=function(t){return function(e){return function(r){return function(n){var a=UC(t)(e),u=function(f){return function(i){return Wt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(rt)(m(i.deleteFromCache({id:_})))(v)})(Tt(xt)(m)(I(O(c))(wt(c)(i.makePeaking({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,q:a.q,gain:a.gain})))(I(O(c))(hr(S(o))(p(k)(function(v){return Nr()()()({frequency:or(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),q:or(f.scope)(i)(function(D){return i.setQ(function(l){return{id:_,q:l}}(D))}),gain:or(f.scope)(i)(function(D){return i.setGain(function(l){return{id:_,gain:l}}(D))})})(v)})(r)))(pa({parent:new N(_),scope:f.scope,raiseId:Ot(Hr(zr(Zr)))})(i)(B(n))))))()}})}};return new w(u)}}}},Ic=function(t){return function(e){return function(r){return _O(t)(e)(M(C(c)))(r)}}},yE=function(t){return function(e){return function(r){var n=CD(t)(e),a=function(u){return function(f){return Wt(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(rt)(i(f.deleteFromCache({id:s})))(_)})(Tt(xt)(i)(I(O(c))(wt(c)(f.makeWaveShaper({id:s,parent:u.parent,scope:u.scope,curve:n.curve,oversample:n.oversample})))(pa({parent:new N(s),scope:u.scope,raiseId:Ot(Hr(zr(Zr)))})(f)(B(r)))))()}})}};return new w(a)}}},pO=function(t){return function(e){return function(r){var n=KC(t)(e),a=function(u){return function(f){return Wt(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(rt)(i(f.deleteFromCache({id:s})))(_)})(Tt(xt)(i)(I(O(c))(wt(c)(f.makeConstant({id:s,parent:u.parent,scope:u.scope,offset:n.offset})))(hr(S(o))(p(k)(function(_){return Nr()()()({offset:or(u.scope)(f)(function(v){return f.setOffset(function(D){return{id:s,offset:D}}(v))}),onOff:function(v){return wt(c)(f.setOnOff({id:s,onOff:v}))}})(_)})(r)))))()}})}};return new w(a)}}},Ss=function(t){return pO(t)};function GD(){window.scrollTo(0,0)}var so=function(t){return t.sequential},xn=function(t){return t.parallel};var yn=function(t){return function(e){return function(r){return new w(V(t)("button")(e)(B(r)))}}};var ha=function(){var t={},e="Pure",r="Throw",n="Catch",a="Sync",u="Async",f="Bind",i="Bracket",m="Fork",s="Sequential",_="Map",v="Apply",D="Alt",l="Cons",g="Resume",nt="Release",st="Finalizer",me="Finalized",Qt="Forked",Je="Fiber",vr="Thunk";function _t(Bt,Qe,Br,ir){this.tag=Bt,this._1=Qe,this._2=Br,this._3=ir}function ve(Bt){var Qe=function(Br,ir,ue){return new _t(Bt,Br,ir,ue)};return Qe.tag=Bt,Qe}function Te(Bt){return new _t(e,void 0)}function re(Bt){try{Bt()}catch(Qe){setTimeout(function(){throw Qe},0)}}function Wr(Bt,Qe,Br){try{return Qe(Br())}catch(ir){return Bt(ir)}}function Ao(Bt,Qe,Br){try{return Qe(Br)()}catch(ir){return Br(Bt(ir))(),Te}}var Bu=function(){var Bt=1024,Qe=0,Br=0,ir=new Array(Bt),ue=!1;function Et(){var Ze;for(ue=!0;Qe!==0;)Qe--,Ze=ir[Br],ir[Br]=void 0,Br=(Br+1)%Bt,Ze();ue=!1}return{isDraining:function(){return ue},enqueue:function(Ze){var Oe,Rr;Qe===Bt&&(Rr=ue,Et(),ue=Rr),ir[(Br+Qe)%Bt]=Ze,Qe++,ue||Et()}}}();function Fi(Bt){var Qe={},Br=0,ir=0;return{register:function(ue){var Et=Br++;ue.onComplete({rethrow:!0,handler:function(Ze){return function(){ir--,delete Qe[Et]}}})(),Qe[Et]=ue,ir++},isEmpty:function(){return ir===0},killAll:function(ue,Et){return function(){if(ir===0)return Et();var Ze=0,Oe={};function Rr(Dr){Oe[Dr]=Qe[Dr].kill(ue,function(Kr){return function(){delete Oe[Dr],Ze--,Bt.isLeft(Kr)&&Bt.fromLeft(Kr)&&setTimeout(function(){throw Bt.fromLeft(Kr)},0),Ze===0&&Et()}})()}for(var Un in Qe)Qe.hasOwnProperty(Un)&&(Ze++,Rr(Un));return Qe={},Br=0,ir=0,function(Dr){return new _t(a,function(){for(var Kr in Oe)Oe.hasOwnProperty(Kr)&&Oe[Kr]()})}}}}}var ru=0,cn=1,Jo=2,qf=3,Hf=4,xr=5,jo=6;function zf(Bt,Qe,Br){var ir=0,ue=ru,Et=Br,Ze=null,Oe=null,Rr=null,Un=null,Dr=null,Kr=0,cf=0,Cu=null,$i=!0;function Mi(oe){for(var fe,Ue,je;;)switch(fe=null,Ue=null,je=null,ue){case Jo:ue=cn;try{Et=Rr(Et),Un===null?Rr=null:(Rr=Un._1,Un=Un._2)}catch(ta){ue=xr,Ze=Bt.left(ta),Et=null}break;case qf:Bt.isLeft(Et)?(ue=xr,Ze=Et,Et=null):Rr===null?ue=xr:(ue=Jo,Et=Bt.fromRight(Et));break;case cn:switch(Et.tag){case f:Rr&&(Un=new _t(l,Rr,Un)),Rr=Et._2,ue=cn,Et=Et._1;break;case e:Rr===null?(ue=xr,Et=Bt.right(Et._1)):(ue=Jo,Et=Et._1);break;case a:ue=qf,Et=Wr(Bt.left,Bt.right,Et._1);break;case u:ue=Hf,Et=Ao(Bt.left,Et._1,function(ta){return function(){ir===oe&&(ir++,Bu.enqueue(function(){ir===oe+1&&(ue=qf,Et=ta,Mi(ir))}))}});return;case r:ue=xr,Ze=Bt.left(Et._1),Et=null;break;case n:Rr===null?Dr=new _t(l,Et,Dr,Oe):Dr=new _t(l,Et,new _t(l,new _t(g,Rr,Un),Dr,Oe),Oe),Rr=null,Un=null,ue=cn,Et=Et._1;break;case i:Kr++,Rr===null?Dr=new _t(l,Et,Dr,Oe):Dr=new _t(l,Et,new _t(l,new _t(g,Rr,Un),Dr,Oe),Oe),Rr=null,Un=null,ue=cn,Et=Et._1;break;case m:ue=qf,fe=zf(Bt,Qe,Et._2),Qe&&Qe.register(fe),Et._1&&fe.run(),Et=Bt.right(fe);break;case s:ue=cn,Et=ph(Bt,Qe,Et._1);break}break;case xr:if(Rr=null,Un=null,Dr===null)ue=jo,Et=Oe||Ze||Et;else switch(fe=Dr._3,je=Dr._1,Dr=Dr._2,je.tag){case n:Oe&&Oe!==fe&&Kr===0?ue=xr:Ze&&(ue=cn,Et=je._2(Bt.fromLeft(Ze)),Ze=null);break;case g:Oe&&Oe!==fe&&Kr===0||Ze?ue=xr:(Rr=je._1,Un=je._2,ue=Jo,Et=Bt.fromRight(Et));break;case i:Kr--,Ze===null&&(Ue=Bt.fromRight(Et),Dr=new _t(l,new _t(nt,je._2,Ue),Dr,fe),(Oe===fe||Kr>0)&&(ue=cn,Et=je._3(Ue)));break;case nt:Dr=new _t(l,new _t(me,Et,Ze),Dr,Oe),ue=cn,Oe&&Oe!==fe&&Kr===0?Et=je._1.killed(Bt.fromLeft(Oe))(je._2):Ze?Et=je._1.failed(Bt.fromLeft(Ze))(je._2):Et=je._1.completed(Bt.fromRight(Et))(je._2),Ze=null,Kr++;break;case st:Kr++,Dr=new _t(l,new _t(me,Et,Ze),Dr,Oe),ue=cn,Et=je._1;break;case me:Kr--,ue=xr,Et=je._1,Ze=je._2;break}break;case jo:for(var Ur in Cu)Cu.hasOwnProperty(Ur)&&($i=$i&&Cu[Ur].rethrow,re(Cu[Ur].handler(Et)));Cu=null,Oe&&Ze?setTimeout(function(){throw Bt.fromLeft(Ze)},0):Bt.isLeft(Et)&&$i&&setTimeout(function(){if($i)throw Bt.fromLeft(Et)},0);return;case ru:ue=cn;break;case Hf:return}}function Gr(oe){return function(){if(ue===jo)return $i=$i&&oe.rethrow,oe.handler(Et)(),function(){};var fe=cf++;return Cu=Cu||{},Cu[fe]=oe,function(){Cu!==null&&delete Cu[fe]}}}function De(oe,fe){return function(){if(ue===jo)return fe(Bt.right(void 0))(),function(){};var Ue=Gr({rethrow:!1,handler:function(){return fe(Bt.right(void 0))}})();switch(ue){case ru:Oe=Bt.left(oe),ue=jo,Et=Oe,Mi(ir);break;case Hf:Oe===null&&(Oe=Bt.left(oe)),Kr===0&&(ue===Hf&&(Dr=new _t(l,new _t(st,Et(oe)),Dr,Oe)),ue=xr,Et=null,Ze=null,Mi(++ir));break;default:Oe===null&&(Oe=Bt.left(oe)),Kr===0&&(ue=xr,Et=null,Ze=null)}return Ue}}function Re(oe){return function(){var fe=Gr({rethrow:!1,handler:oe})();return ue===ru&&Mi(ir),fe}}return{kill:De,join:Re,onComplete:Gr,isSuspended:function(){return ue===ru},run:function(){ue===ru&&(Bu.isDraining()?Mi(ir):Bu.enqueue(function(){Mi(ir)}))}}}function Xo(Bt,Qe,Br,ir){var ue=0,Et={},Ze=0,Oe={},Rr=new Error("[ParAff] Early exit"),Un=null,Dr=t;function Kr(Gr,De,Re){var oe=De,fe=null,Ue=null,je=0,Ur={},ta,rl;t:for(;;)switch(ta=null,oe.tag){case Qt:if(oe._3===t&&(ta=Et[oe._1],Ur[je++]=ta.kill(Gr,function(sh){return function(){je--,je===0&&Re(sh)()}})),fe===null)break t;oe=fe._2,Ue===null?fe=null:(fe=Ue._1,Ue=Ue._2);break;case _:oe=oe._2;break;case v:case D:fe&&(Ue=new _t(l,fe,Ue)),fe=oe,oe=oe._1;break}if(je===0)Re(Bt.right(void 0))();else for(rl=0,ta=je;rl<ta;rl++)Ur[rl]=Ur[rl]();return Ur}function cf(Gr,De,Re){var oe,fe,Ue,je,Ur,ta;Bt.isLeft(Gr)?(oe=Gr,fe=null):(fe=Gr,oe=null);t:for(;;){if(Ue=null,je=null,Ur=null,ta=null,Un!==null)return;if(De===null){ir(oe||fe)();return}if(De._3!==t)return;switch(De.tag){case _:oe===null?(De._3=Bt.right(De._1(Bt.fromRight(fe))),fe=De._3):De._3=oe;break;case v:if(Ue=De._1._3,je=De._2._3,oe){if(De._3=oe,Ur=!0,ta=Ze++,Oe[ta]=Kr(Rr,oe===Ue?De._2:De._1,function(){return function(){delete Oe[ta],Ur?Ur=!1:Re===null?cf(oe,null,null):cf(oe,Re._1,Re._2)}}),Ur){Ur=!1;return}}else{if(Ue===t||je===t)return;fe=Bt.right(Bt.fromRight(Ue)(Bt.fromRight(je))),De._3=fe}break;case D:if(Ue=De._1._3,je=De._2._3,Ue===t&&Bt.isLeft(je)||je===t&&Bt.isLeft(Ue))return;if(Ue!==t&&Bt.isLeft(Ue)&&je!==t&&Bt.isLeft(je))oe=fe===Ue?je:Ue,fe=null,De._3=oe;else if(De._3=fe,Ur=!0,ta=Ze++,Oe[ta]=Kr(Rr,fe===Ue?De._2:De._1,function(){return function(){delete Oe[ta],Ur?Ur=!1:Re===null?cf(fe,null,null):cf(fe,Re._1,Re._2)}}),Ur){Ur=!1;return}break}Re===null?De=null:(De=Re._1,Re=Re._2)}}function Cu(Gr){return function(De){return function(){delete Et[Gr._1],Gr._3=De,cf(De,Gr._2._1,Gr._2._2)}}}function $i(){var Gr=cn,De=Br,Re=null,oe=null,fe,Ue;t:for(;;)switch(fe=null,Ue=null,Gr){case cn:switch(De.tag){case _:Re&&(oe=new _t(l,Re,oe)),Re=new _t(_,De._1,t,t),De=De._2;break;case v:Re&&(oe=new _t(l,Re,oe)),Re=new _t(v,t,De._2,t),De=De._1;break;case D:Re&&(oe=new _t(l,Re,oe)),Re=new _t(D,t,De._2,t),De=De._1;break;default:Ue=ue++,Gr=xr,fe=De,De=new _t(Qt,Ue,new _t(l,Re,oe),t),fe=zf(Bt,Qe,fe),fe.onComplete({rethrow:!1,handler:Cu(De)})(),Et[Ue]=fe,Qe&&Qe.register(fe)}break;case xr:if(Re===null)break t;Re._1===t?(Re._1=De,Gr=cn,De=Re._2,Re._2=t):(Re._2=De,De=Re,oe===null?Re=null:(Re=oe._1,oe=oe._2))}for(Dr=De,Ue=0;Ue<ue;Ue++)Et[Ue].run()}function Mi(Gr,De){Un=Bt.left(Gr);var Re;for(var oe in Oe)if(Oe.hasOwnProperty(oe)){Re=Oe[oe];for(oe in Re)Re.hasOwnProperty(oe)&&Re[oe]()}Oe=null;var fe=Kr(Gr,Dr,De);return function(Ue){return new _t(u,function(je){return function(){for(var Ur in fe)fe.hasOwnProperty(Ur)&&fe[Ur]();return Te}})}}return $i(),function(Gr){return new _t(u,function(De){return function(){return Mi(Gr,De)}})}}function ph(Bt,Qe,Br){return new _t(u,function(ir){return function(){return Xo(Bt,Qe,Br,ir)}})}return _t.EMPTY=t,_t.Pure=ve(e),_t.Throw=ve(r),_t.Catch=ve(n),_t.Sync=ve(a),_t.Async=ve(u),_t.Bind=ve(f),_t.Bracket=ve(i),_t.Fork=ve(m),_t.Seq=ve(s),_t.ParMap=ve(_),_t.ParApply=ve(v),_t.ParAlt=ve(D),_t.Fiber=zf,_t.Supervisor=Fi,_t.Scheduler=Bu,_t.nonCanceler=Te,_t}(),kE=ha.Pure,yO=ha.Throw;function gE(t){return function(e){return e.tag===ha.Pure.tag?ha.Pure(t(e._1)):ha.Bind(e,function(r){return ha.Pure(t(r))})}}function CE(t){return function(e){return ha.Bind(t,e)}}var EE=ha.Sync;function SE(t){return function(e){return ha.ParMap(t,e)}}function hE(t){return function(e){return ha.ParApply(t,e)}}function TE(t){return function(e){return ha.ParAlt(t,e)}}var Rc=ha.Async;function xE(t,e){return function(){return ha.Fiber(t,null,e)}}var kO=function(){function t(r,n){return r===0&&typeof setImmediate<"u"?setImmediate(n):setTimeout(n,r)}function e(r,n){return r===0&&typeof clearImmediate<"u"?clearImmediate(n):clearTimeout(n)}return function(r,n){return ha.Async(function(a){return function(){var u=t(n,a(r()));return function(){return ha.Sync(function(){return r(e(n,u))})}}})}}(),FE=ha.Seq;var CO=function(t){return function(e){return function(r){var n=so(t),a=pr(t.Applicative1())(e)(function(){var u=xn(t);return function(f){return u(r(f))}}());return function(u){return n(a(u))}}}},$E=function(t){return function(e){return function(r){var n=so(t),a=jn(e)(t.Applicative1())(function(){var u=xn(t);return function(f){return u(r(f))}}());return function(u){return n(a(u))}}}},ME=function(t){return function(e){return CO(t)(e)(Z(tt))}};var EO=function(t){return t};var PE=function(t){return t};var T_=function(t){return t.toDuration};var OE={fromDuration:km()()(EO)(function(t){return t*1e3}),toDuration:km()()(PE)(function(t){return t/1e3})};var IE=function(t,e,r){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return n=1,a=r(),n=2,a}};var hO=function(t){return t};var Lc={map:SE},bi={map:gE};var TO=function(){var t=function(n){if(n instanceof jt)return n.value0;if(n instanceof Jt)return lu("unsafeFromRight: Left");throw new Error("Failed pattern match at Effect.Aff (line 407, column 21 - line 409, column 54): "+[n.constructor.name])},e=function(n){if(n instanceof Jt)return n.value0;if(n instanceof jt)return lu("unsafeFromLeft: Right");throw new Error("Failed pattern match at Effect.Aff (line 402, column 20 - line 404, column 55): "+[n.constructor.name])},r=function(n){if(n instanceof Jt)return!0;if(n instanceof jt)return!1;throw new Error("Failed pattern match at Effect.Aff (line 397, column 12 - line 399, column 21): "+[n.constructor.name])};return{isLeft:r,fromLeft:e,fromRight:t,left:Jt.create,right:jt.create}}(),xO=function(t){return xE(TO,t)},mo=function(t){return function(){var r=xO(t)();return r.run(),r}},Uo=function(){var t=Ae(x);return function(e){return t(mo(e))}}();var Ai={apply:hE,Functor0:function(){return Lc}};var UD={Applicative0:function(){return sa},Bind1:function(){return Or}},Or={bind:CE,Apply0:function(){return qD(0)}},sa={pure:kE,Apply0:function(){return qD(0)}},qD=IE("applyAff","Effect.Aff",function(){return{apply:Uu(UD),Functor0:function(){return bi}}}),RE=qD(71);var Pr={liftEffect:EE,Monad0:function(){return UD}},NE=function(){var t=mr(Pr);return function(e){return hO(T(t(e)))}}(),LE=function(t){return Rc(function(e){return p(x)(NE)(t.join(e))})};var WE=function(t){return function(e){return U(Or)(mr(Pr)(e.isSuspended))(function(r){return r?mr(Pr)(Ae(x)(e.kill(t,T(h(c)(void 0))))):Rc(function(n){return p(x)(NE)(e.kill(t,n))})})}};var Fn={parallel:ct,sequential:FE,Monad0:function(){return UD},Applicative1:function(){return FO(0)}},FO=IE("applicativeParAff","Effect.Aff",function(){return{pure:function(){var t=xn(Fn),e=h(sa);return function(r){return t(e(r))}}(),Apply0:function(){return Ai}}});var $O={append:function(t){return function(e){return function(r){return ME(Fn)(Xt)([t(r),e(r)])}}}};var MO=T(h(sa)(void 0)),BE={mempty:MO,Semigroup0:function(){return $O}};var GE={alt:TE,Functor0:function(){return Lc}};var UE=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),x_=function(){function t(){}return t.value=new t,t}(),Lf=function(){function t(){}return t.value=new t,t}(),F_=function(){function t(){}return t.value=new t,t}(),Wf=function(){function t(){}return t.value=new t,t}(),$_=function(){function t(){}return t.value=new t,t}(),M_=function(){function t(){}return t.value=new t,t}(),qE=function(){function t(){}return t.value=new t,t}(),hs=function(){function t(){}return t.value=new t,t}(),Ts=function(){function t(){}return t.value=new t,t}(),w_=function(){function t(){}return t.value=new t,t}(),P_=function(){function t(){}return t.value=new t,t}(),HE=function(){function t(){}return t.value=new t,t}(),Wc=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),HD=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var wO="numeric",PO="sudden",OO="unit",IO="cancel",RO="step",NO="linear",LO="exponential",WO="envelope",zE=function(t,e,r,n){if(r.type===PO)t.value=r.value.n;else if(r.type===OO)e.id&&GO(e.id,n),n.units[r.value.i].main.connect(t),e.id=r.value.i;else if(r.type===wO)t[r.value.t.type===RO?"setValueAtTime":r.value.t.type===NO?"linearRampToValueAtTime":r.value.t.type===LO?"exponentialRampToValueAtTime":"linearRampToValueAtTime"](r.value.n,r.value.o);else if(r.type===IO)r.value.hold?t.cancelAndHoldAtTime(r.value.o):t.cancelScheduledValues(r.value.o);else if(r.type===WO){let a=r.value.o;t.cancelScheduledValues(Math.max(0,a)),t.setValueCurveAtTime(r.value.p,a,r.value.d)}else throw new Error("No idea what to do with "+JSON.stringify(r))},BO=function(t,e,r,n,a){return n[r]||(n[r]={}),zE(e.parameters.get(r),n[r],a,t)},Nu=function(t,e,r,n,a){return n[r]||(n[r]={}),zE(e[r],n[r],a,t)},gr=function(t,e,r){let n=e.value0?e.value0:"@fan@";r.scopes[n]||(r.scopes[n]=[]),r.scopes[n].push(t),r.units[t].scope=n},Cr=function(t,e){e.toConnect[t]&&(e.toConnect[t].forEach(function(r){r.w?e.units[r.w]?r.f():(e.toConnect[r.w]||(e.toConnect[r.w]=[]),e.toConnect[r.w].push({f:r.f})):r.f()}),delete e.toConnect[t])},Er=function(t,e,r,n){t()(a=>VE(e,a,n))(r)},VE=function(t,e,r){var n=function(){r.units[t].audioOutgoing.push(e),r.units[t].pendingOn||(r.units[t].main.connect(r.units[e].main),r.units[e].se&&r.units[t].main.connect(r.units[e].se))};if(!r.units[t]){r.toConnect[t]||(r.toConnect[t]=[]);var a={f:n};e!==t&&!r.units[e]&&(a.w=e),r.toConnect[t].push(a);return}if(!r.units[e]){r.toConnect[e]||(r.toConnect[e]=[]);var a={f:n};e!==t&&!r.units[t]&&(a.w=t),r.toConnect[e].push(a);return}n()};function zD(t){return function(e){return function(){delete e.units[t.id]}}}function VD(t){return function(e){return function(){VE(t.from,t.to,e)}}}var GO=function(t,e){if(e.units[t].scope==="@fan@")return;let r=e.units[t].scope;e.scopes[r].forEach(n=>{delete e.units[n]}),delete e.scopes[r]};function JD(t){return function(e){return function(){var r=t.from,n=t.to;if(e.units[r].audioOutgoing=e.units[r].audioOutgoing.filter(function(u){return u!==n}),e.units[r].main.disconnect(e.units[n].main),e.units[n].se&&e.units[r].main.disconnect(e.units[n].se),e.units[r].scope==="@fan@")return;let a=e.units[r].scope;e.scopes[a].forEach(u=>{delete e.units[u]}),delete e.scopes[a]}}}var jD=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"allpass",Q:e.q,frequency:e.frequency})},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},XD=t=>e=>r=>()=>{var n=e.id,a=e.cb,u=new AnalyserNode(r.context,e),f=a(u)();r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],analyserOrig:a,analyser:f,main:r.context.createGain(),se:u},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},QD=t=>e=>r=>()=>{var n=e.id,a=e.options;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new AudioWorkletNode(r.context,a.name,{numberOfInputs:a.numberOfInputs,numberOfOutputs:a.numberOfOutputs,outputChannelCount:a.outputChannelCount,parameterData:a.parameterData,processorOptions:a.processorOptions})},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},KD=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"bandpass",Q:e.q,frequency:e.frequency})},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},YD=t=>e=>r=>()=>{var n=e.id,a=function(f,i){return new ConstantSourceNode(f,i)},u={offset:e.offset};r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},ZD=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new ConvolverNode(r.context,{buffer:e.buffer})},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},td=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DelayNode(r.context,{delayTime:e.delayTime,maxDelayTime:e.maxDelayTime})},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},ed=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DynamicsCompressorNode(r.context,{knee:e.knee,ratio:e.ratio,threshold:e.threshold,attack:e.attack,release:e.release})},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},rd=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new GainNode(r.context,{gain:e.gain})},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},nd=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"highpass",Q:e.q,frequency:e.frequency})},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},ad=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"highshelf",frequency:e.frequency,gain:e.gain})},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},ud=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new IIRFilterNode(r.context,{feedforward:e.feedforward,feedback:e.feedback})},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},od=t=>e=>r=>()=>{var n=e.id,a=function(f,i){return new AudioBufferSourceNode(f,i)},u={loop:!0,buffer:e.buffer,loopStart:e.loopStart,loopEnd:e.loopEnd,playbackRate:e.playbackRate};r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},id=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"lowpass",Q:e.q,frequency:e.frequency})},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},fd=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"lowshelf",frequency:e.frequency,gain:e.gain})},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},cd=t=>e=>r=>()=>{var n=e.id,a=e.element,u=function(){var f=r.context.createMediaElementSource(a);return f};r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],createClosure:u,resumeClosure:{},main:u()},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},ld=t=>e=>r=>()=>{var n=e.id;r.units[e.id]={main:r.context.createMediaStreamSource(e.microphone),controllers:{},audioOutgoing:[],controlOutgoing:[]},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},_d=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"notch",frequency:e.frequency,Q:e.q})},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},pd=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"peaking",frequency:e.frequency,Q:e.q,gain:e.gain})},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},sd=t=>e=>r=>()=>{var n=e.id,a=function(f,i){var m={frequency:i.frequency,periodicWave:i.spec.type==="wave"?i.spec.value:Kd(r.context)(i.spec.value.real)(i.spec.value.img)()},s=new OscillatorNode(f,m);return s},u={frequency:e.frequency,type:"custom",spec:e.spec};r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},md=t=>e=>r=>()=>{var n=e.id,a=function(f,i){var m={loop:i.loop,buffer:i.buffer,playbackRate:i.playbackRate};return new AudioBufferSourceNode(f,m)},u={loop:!1,buffer:e.buffer,playbackRate:e.playbackRate,bufferOffset:e.bufferOffset,duration:t(void 0)(f=>f)(e.duration)};r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},vd=t=>e=>r=>()=>{var n=e.id,a=e.cb,u=r.context.createMediaStreamDestination(),f=new MediaRecorder(u.stream);a(f)(),f.start(),r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],recorderOrig:a,recorder:f,main:r.context.createGain(),se:u},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},Dd=t=>e=>r=>()=>{var n=e.id,a=function(f,i){return new OscillatorNode(f,i)},u={frequency:e.frequency,type:"sawtooth"};r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},dd=t=>e=>r=>()=>{var n=e.id,a=function(f,i){return new OscillatorNode(f,i)},u={frequency:e.frequency,type:"sine"};r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},bd=t=>e=>()=>{e.units[t.id]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:e.context.createGain(),se:e.context.destination}},Ad=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new StereoPannerNode(r.context,{pan:e.pan})},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},yd=t=>e=>r=>()=>{var n=e.id,a=function(f,i){return new OscillatorNode(f,i)},u={frequency:e.frequency,type:"square"};r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},kd=t=>e=>r=>()=>{var n=e.id,a=function(f,i){return new OscillatorNode(f,i)},u={frequency:e.frequency,type:"triangle"};r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)},gd=t=>e=>r=>()=>{var n=e.id,a=e.curve,u=e.oversample;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new WaveShaperNode(r.context,{curve:a,oversample:u.type})},gr(n,e.scope,r),Cr(n,r),Er(t,n,e.parent,r)};function Cd(t){return function(e){return function(){var r=t.id,n=t.cb;e.units[r].analyserOrig!==n&&(e.units[r].analyser&&e.units[r].analyser(),e.units[r].analyser=n(e.units[r].se)(),e.units[r].analyserOrig=n)}}}function Ed(t){return function(e){return function(){var r=t.cb,n=t.id;if(e.units[n].recorderOrig!==r){e.units[n].recorder&&e.units[n].recorder.stop();var a=r;e.units[n].recorderOrig=r;var u=new MediaRecorder(e.units[n].se);a(u)(),u.start()}}}}function Sd(t){return function(e){return function(){var r=t.id,n=t.curve;e.units[r].main.curve=n}}}function hd(t){return function(e){return function(){var r=t.id,n=t.paramName,a=t.paramValue;BO(e,e.units[r].main,n,e.units[r].controllers,a)}}}var Lu=function(t,e,r){e.resume&&t.value.n!==void 0&&(e.resume[r]=t.value.n)};function Td(t){return function(e){return function(){var r=t.id,n=t.gain;Nu(e,e.units[r].main,"gain",e.units[r].controllers,n),Lu(n,e.units[r],"gain")}}}function xd(t){return function(e){return function(){var r=t.id,n=t.q;Nu(e,e.units[r].main,"Q",e.units[r].controllers,n),Lu(n,e.units[r],"Q")}}}function Fd(t){return function(e){return function(){var r=t.id,n=t.buffer;e.units[r].resume&&(e.units[r].resume.buffer=n)}}}function $d(t){return function(e){return function(){var r=t.id,n=t.buffer;e.units[r].main.buffer=n}}}function Md(t){return function(e){return function(){var r=t.id,n=t.spec;e.units[r].resume&&(e.units[r].resume.spec=n)}}}function wd(t){return function(e){return function(){var r=t.id,n=t.pan;Nu(e,e.units[r].main,"pan",e.units[r].controllers,n),Lu(n,e.units[r],"pan")}}}function Pd(t){return function(e){return function(){var r=t.id,n=t.threshold;Nu(e,e.units[r].main,"threshold",e.units[r].controllers,n),Lu(n,e.units[r],"threshold")}}}function Od(t){return function(e){return function(){var r=t.id,n=t.loopStart;e.units[r].main.loopStart=n,e.units[r].resume.loopStart=n}}}function Id(t){return function(e){return function(){var r=t.id,n=t.loopEnd;e.units[r].main.loopEnd=n,e.units[r].resume.loopEnd=n}}}function Rd(t){return function(e){return function(){var r=t.id,n=t.bufferOffset;e.units[r].resume.bufferOffset=n}}}function Nd(t){return function(e){return function(r){return function(){var n=e.id,a=e.duration;r.units[n].duration=t(void 0)(u=>u)(a)}}}}function Ld(t){return function(e){return function(){var r=t.id,n=t.release;Nu(e,e.units[r].main,"release",e.units[r].controllers,n),Lu(n,e.units[r],"release")}}}function Wd(t){return function(e){return function(){var r=t.id,n=t.offset;Nu(e,e.units[r].main,"offset",e.units[r].controllers,n),Lu(n,e.units[r],"offset")}}}function Bd(t){return function(e){return function(){var r=t.id,n=t.ratio;Nu(e,e.units[r].main,"ratio",e.units[r].controllers,n),Lu(n,e.units[r],"ratio")}}}function Gd(t){return function(e){return function(){var r=t.id,n=t.attack;Nu(e,e.units[r].main,"attack",e.units[r].controllers,n),Lu(n,e.units[r],"attack")}}}function Ud(t){return function(e){return function(){var r=t.id,n=t.knee;Nu(e,e.units[r].main,"knee",e.units[r].controllers,n),Lu(n,e.units[r],"knee")}}}function qd(t){return function(e){return function(){var r=t.id,n=t.delayTime;Nu(e,e.units[r].main,"delayTime",e.units[r].controllers,n),Lu(n,e.units[r],"delayTime")}}}function Hd(t){return function(e){return function(){var r=t.id,n=t.playbackRate;Nu(e,e.units[r].main,"playbackRate",e.units[r].controllers,n),Lu(n,e.units[r],"playbackRate")}}}function zd(t){return function(e){return function(){var r=t.id,n=t.frequency;Nu(e,e.units[r].main,"frequency",e.units[r].controllers,n),Lu(n,e.units[r],"frequency")}}}function Vd(t){return function(e){return function(){var r=t.id,n=t.onOff;n.x.type==="on"?UO(r)(n)(e)():n.x.type==="off"&&qO(r)(n)(e)()}}}var UO=function(t){return function(e){return function(r){return function(){if(!r.units[t].onOff){r.units[t].pendingOn=!1,r.units[t].onOff=!0,r.units[t].main=r.units[t].createClosure(r.context,r.units[t].resume);for(var n=0;n<r.units[t].audioOutgoing.length;n++){var a=r.units[t].audioOutgoing[n];r.units[t].main.connect(r.units[a].main),r.units[a].se&&r.units[t].main.connect(r.units[a].se)}r.units[t].resume&&r.units[t].resume.bufferOffset?typeof r.units[t].resume.duration=="number"?r.units[t].main.start(r.deprecatedWriteHead+e.o,r.units[t].resume.bufferOffset,r.units[t].resume.duration):r.units[t].main.start(r.deprecatedWriteHead+e.o,r.units[t].resume.bufferOffset):r.units[t].resume&&r.units[t].resume.loopStart?r.units[t].main.start(r.deprecatedWriteHead+e.o,r.units[t].resume.loopStart):r.units[t].main.start(r.deprecatedWriteHead+e.o)}}}}},qO=function(t){return function(e){return function(r){return function(){if(!!r.units[t].onOff){r.units[t].onOff=!1;var n=r.units[t].main;n.addEventListener("ended",()=>{n.disconnect()}),n.stop(r.deprecatedWriteHead+e.o)}}}}};function Jd(t){for(var e=new Float32Array(t.length),r=0;r<t.length;r++)e[r]=t[r];return e}function xs(t){return function(){t.stop()}}function jd(t){return function(e){return function(r){return function(){var n=[];r.ondataavailable=function(a){n.push(a.data)},r.onstop=function(){var a=new Blob(n,{type:t});e(a)(),n=null}}}}}function Xd(t){return function(e){return function(){return navigator.mediaDevices.getUserMedia({audio:t,video:e})}}}function O_(t){return function(){var e=new Uint8Array(t.frequencyBinCount);return t.getByteFrequencyData(e),e}}function Qd(t){return function(){var e=t.createConstantSource();return e.offset.value=0,e.connect(t.destination),e.start(),function(){e.stop(),e.disconnect(t.destination)}}}var Kd=function(t){return function(e){return function(r){return function(){for(var n=new Float32Array(e.length),a=new Float32Array(r.length),u=0;u<e.length;u++)n[u]=e[u];for(var u=0;u<r.length;u++)a[u]=r[u];return t.createPeriodicWave(n,a,{disableNormalization:!0})}}}};function Zi(t){return function(){return{context:t,deprecatedWriteHead:0,units:{},scopes:{},unsu:{},toConnect:{}}}}function Yd(t){return function(){t.close()}}function Zd(t){return function(){return fetch(t).then(function(e){return e.arrayBuffer()},function(e){return console.error("Error fetching buffer",e),Promise.reject(e)})}}function tb(t){return function(e){return function(){return t.decodeAudioData(e)}}}function eb(){return new(window.AudioContext||window.webkitAudioContext)}function rb(t){return function(){return t.state}}function I_(t){return function(){return t.currentTime}}function JE(t){return function(e){return function(r){return function(){t.then(r,e)}}}}var VO=function(t){return function(e){return Rc(function(r){return J_(x)(Ot(BE))(JE(e)(function(n){return r(Jt.create(t(n)))()})(function(n){return r(jt.create(n))()}))})}};var JO=function(t){return Pa(function(e){return Io("Promise failed, couldn't extract JS Error or String")})(Z(tt))(nD(I(rD(Lm)(Hu))(lD(Hu)("Error")(t))(p(__(Co))(Io)(_D(Hu)(t)))))},jE=VO(JO),Fs=function(t){return U(Or)(mr(Pr)(t))(jE)};function nb(t){return function(){return URL.createObjectURL(t)}}var XE=function(t){return function(e){return function(r){return Tt(jd(t))(r)(function(){var n=qn(Cn)(e);return function(a){return n(nb(a))}}())}}};var Bf={ids:p(x)(zt(X_))(Ou),deleteFromCache:zD,disconnectXFromY:JD,connectXToY:VD,makeAllpass:jD(Kt),makeAnalyser:XD(Kt),makeAudioWorkletNode:QD(Kt),makeBandpass:KD(Kt),makeConstant:YD(Kt),makeConvolver:ZD(Kt),makeDelay:td(Kt),makeDynamicsCompressor:ed(Kt),makeGain:rd(Kt),makeHighpass:nd(Kt),makeHighshelf:ad(Kt),makeIIRFilter:ud(Kt),makeLoopBuf:od(Kt),makeLowpass:id(Kt),makeLowshelf:fd(Kt),makeMediaElement:cd(Kt),makeMicrophone:ld(Kt),makeNotch:_d(Kt),makePeaking:pd(Kt),makePeriodicOsc:sd(Kt),makePlayBuf:md(Kt),makeRecorder:vd(Kt),makeSawtoothOsc:Dd(Kt),makeSinOsc:dd(Kt),makeSpeaker:bd,makeSquareOsc:yd(Kt),makeStereoPanner:Ad(Kt),makeTriangleOsc:kd(Kt),makeWaveShaper:gd(Kt),setAnalyserNodeCb:Cd,setMediaRecorderCb:Ed,setWaveShaperCurve:Sd,setAudioWorkletParameter:hd,setBuffer:Fd,setConvolverBuffer:$d,setDuration:Nd(Kt),setPeriodicOsc:Md,setOnOff:Vd,setBufferOffset:Rd,setLoopStart:Od,setLoopEnd:Id,setRatio:Bd,setOffset:Wd,setAttack:Gd,setGain:Td,setQ:xd,setPan:wd,setThreshold:Pd,setRelease:Ld,setKnee:Ud,setDelay:qd,setPlaybackRate:Hd,setFrequency:zd},Ct=function(t){return function(e){return U(Or)(Fs(Zd(e)))(function(){var r=tb(t);return function(n){return Fs(r(n))}}())}},R_=function(t){var e=mr(t);return function(r){return e(rb(r))}};var Yn=function(t){return mr(t)(eb)},Wu=function(t){var e=mr(t);return function(r){return e(Qd(r))}},kn=function(t){return function(e){return mr(t)(function(){var n=R_(ur)(e)();return Mn(c)(n!=="closed")(Yd(e))()})}},YO=ct,ZO=ct,$s=function(t){return function(e){return p(bi)(function(r){return{microphone:function(){return t?h(go)(YO(r)):z.value}(),camera:function(){return e?h(go)(ZO(r)):z.value}()}})(Fs(Xd(t)(e)))}};var qo=function(){function t(){}return t.value=new t,t}(),Ho=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),gu=function(){function t(){}return t.value=new t,t}(),fn=GD,yi=function(t){return so(Fn)(I(GE)(xn(Fn)(U(Or)(LE(t))(mr(Pr))))(xn(Fn)(WE(Io("We navigated away from the page"))(t))))},Bc=function(t){return function(e){return function(r){return function(n){return I(t)(K(e)(gu.value))(n)}}}},Ta=function(t){return function(e){return function(r){return function(n){return I(t)(K(e)(Y(sr)(lr.value)(Ke(T(n)))))(p(t.Functor0())(function(a){return Y(sr)(lr.value)(Ke(T(j(rt)(a)(n))))})(p(t.Functor0())(function(a){return a.value0})(r)))}}}},Ms=function(t){return function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return function(i){return function(m){return p(t)(function(s){return Y(sr)(lr.value)(Ke(T(function(){if(s.value0 instanceof qo)return h(c)(void 0);if(s.value0 instanceof Ho)return j(rt)(j(rt)(s.value0.value0)(n(h(c)(void 0))))(a(gu.value));if(s.value0 instanceof gu)return function(){s.value1(),a(qo.value)();var v=mo(U(Or)(Yn(Pr))(function(D){return U(Or)(Wu(Pr)(D))(function(l){return U(Or)(u(D))(function(g){return mr(Pr)(function(){var st=f(D)(g)(),me=j(rt)(j(rt)(st)(l))(kn(ur)(D));return a(new Ho(me))(),me})})})}))();return er(e)(Cn)(n(function(){return a(gu.value)(),Uo(yi(v))()}))(function(){return h(c)(void 0)})()};throw new Error("Failed pattern match at WAGS.Example.Docs.Util (line 48, column 21 - line 66, column 26): "+[s.value0.constructor.name])}())))})(Sn(r)(I(r.Plus0().Alt0())(K(r)(h(c)(void 0)))(p(t)(function(s){return s.value0})(i)))(p(t)(ut.create)(m)))}}}}}}}}},xa=function(t){return function(e){return function(r){return function(){return t(r)(),e(new UE(r))()}}}},ws=function(t){return function(e){return function(r){return function(n){return function(a){return jr(o)(function(u){return function(f){var i=Bc(O(c))(S(o))(e)(f);return lc(o)(I(O(c))(K(S(o))(Y(Pp)(qt.value)("cursor: pointer;")))(Ms(k)(Xe)(S(o))(r)(u)(n)(a)(e)(i)))([nn(ae)(p(k)(function(m){if(m instanceof gu)return t;if(m instanceof qo)return"\u23F3";if(m instanceof Ho)return"\u{1F6D1}";throw new Error("Failed pattern match at WAGS.Example.Docs.Util (line 115, column 19 - line 118, column 37): "+[m.constructor.name])})(i))])}})}}}}},St=function(t){return function(e){return function(r){return function(n){return jr(o)(function(a){return function(u){var f=Bc(O(c))(S(o))(t)(u);return yn(o)(Ms(k)(Xe)(S(o))(e)(a)(r)(n)(t)(f))([nn(ae)(p(k)(function(i){if(i instanceof gu)return"Turn on";if(i instanceof qo)return"Loading...";if(i instanceof Ho)return"Turn off";throw new Error("Failed pattern match at WAGS.Example.Docs.Util (line 89, column 19 - line 92, column 44): "+[i.constructor.name])})(f))])}})}}}};var Gc=function(t){return function(e){return function(){var n=Zi(t)(),a=xt(Rf([new oc(p(k)(function(u){return gf.create(qy(u))})(e))])(Bf))(function(u){return u(n)})();return a}}};var vt=function(t){return function(e){return function(){var n=Zi(t)(),a=xt(Rf(e)(Bf))(function(u){return u(n)})();return a}}},Ps=function(t){return function(){var r=Yn(ur)();return p(x)(function(n){return j(rt)(n)(kn(ur)(r))})(vt(r)(t))()}};var tI=function(){return d.value}(),QE=function(t){return function(e){return function(r){return _e({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(o)(W()(Q(c))({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}})(o))(d.value)(tI)({allpass:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([Sa(ie(ge)(a)(lt()))(function(u){return function(f){return Pt(ot)(.2)([u,E_(ID)(700)([E_(bs(kt(yt()(J(J(At)(gD)()()()({reflectSymbol:function(){return"q"}}))(Ds)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:990,q:20})([u]),E_(ID)(1110)([u,E_(bs(kt(yt()(J(J(At)(gD)()()()({reflectSymbol:function(){return"q"}}))(Ds)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:2010,q:30})([u])])])])}})])}})))})}}};function ki(t){return function(r,n,a){if(n===null)return new t(r);var u=r.byteLength,f=t.BYTES_PER_ELEMENT,i=Math.min(u,n>>>0);if(a===null)return new t(r,i);var m=Math.min((u-i)/f,a);return new t(r,i,m)}}var rI=ki(Uint8ClampedArray),nI=ki(Uint32Array),aI=ki(Uint16Array),KE=ki(Uint8Array),uI=ki(Int32Array),oI=ki(Int16Array),iI=ki(Int8Array),fI=ki(Float32Array),cI=ki(Float64Array);function YE(t){for(var e=t.length,r=new Array(e),n=0;n<e;n++)r[n]=t[n];return r}var Os={create:KE,BinaryValue0:function(){}};var Is=function(t){return function(e){return function(){return YE(e)}}};var Uc=Du,qc=Du,Hc=Du,Xa=Du,Qa=Du,Ka=Du,Ya=Du,Za=Du;function Rs(t){return t|0}var gi=function(){return window};function rS(t,e,r,n){if(typeof window<"u"){var a=window[r];if(a!=null&&n instanceof a)return e(n)}for(var u=n;u!=null;){var f=Object.getPrototypeOf(u),i=f.constructor.name;if(i===r)return e(n);if(i==="Object")return t;u=f}return t}var It=function(t){return function(e){return rS(z.value,N.create,t,e)}};var ab=It("HTMLCanvasElement");function nS(t){return function(){return t.body}}var aS=function(){var t=p(x)(Xr);return function(e){return t(nS(e))}}();var uS=ct;function Gf(t){return function(){return t.valueAsNumber}}var zc=It("HTMLInputElement");function ob(t){return function(){return t.document}}function Ns(t){return function(e){return function(){return e.requestAnimationFrame(t)}}}var ib=ct;var QI=function(t,e,r){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return n=1,a=r(),n=2,a}},tf=Wt(function(t){return function(){var r=gi(),n=cr(!0)(),a=QI("fx","FRP.Event.Animate",function(){return Ae(x)(Tt(Ns)(r)(function(){var i=Mr(n)();return Mn(c)(i)(function(){return t(void 0)(),a(19)()})()}))}),u=a(15);return u(),En(!1)(n)}});var KI="background-color: rgb(150,30,10);",YI="background-color: rgb(130,60,10);",ZI="background-color: rgb(80,90,10);",tR="background-color: rgb(10,130,10);",eR="background-color: rgb(10,100,0);",rR=rs(Va)(function(t){return we(Me(ts)()(Wa)()(c_))(KI)(we(Me(Kn)()(sn)()(Wa))(YI)(we(Me(du)()(mn)()(sn))(ZI)(we(Me(bu)()(vn)()(mn))(tR)(we(Me(Au)()(yu)()(vn))(eR)(Ru)))))}),nR=function(t){return function(e){return function(r){return function(n){return Cs(gs(kt(yt()(J(J(At)(ks)()()()({reflectSymbol:function(){return"fftSize"}}))(t)()()()({reflectSymbol:function(){return"cb"}})))(bt()())))({cb:n,fftSize:ls.value})([ie(e)(r)(lt())])}}}},aR=function(){return d.value}(),Ie="background-color: rgb(255,255,255,0.0);",Ne=function(t){return function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return function(i){return function(m){return function(s){return function(_){return p(t)(function(v){var D=l_(e)()(l_(n)()(v)(m))(s);return D?Y(u)(qt.value)(l_(e)()(l_(n)()(rR)(m))(s)):Y(u)(qt.value)(Ie)})(_)}}}}}}}}}}},uR=function(){return 15/40}(),oR=function(){return 10/40}(),iR=function(){return 7/40}(),fR=function(){return 3/40}(),cR=function(){return 1/40}(),iS=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
  <h2 id="analyser">Analyser</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode">analyser node</a> provides methods to recuperate the analysed data of an input. This is how, for example, Google Meet shows the little animation around a microphone icon. Wags provides the possibility to use the analyser as the terminus of an audio graph <i>or</i> as part of a longer DSP chain, as in the following example. The example uses an FFT size of 256, which is indicated in Wags as <code>TTT8</code> (two to the eighth power).</p>

  <pre><code>analyser_ { cb, fftSize: TTT8 } [ loopBuf atar bangOn ]</code></pre>

  ~analyser~
  </section>
`}})()()(W()(Q(c))({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}})(o))(o)(aR)({analyser:L(et(jr(o)(function(n){return function(a){var u=$l(pu(c))(Z(tt))(a),f=Bc(O(c))(S(o))(r)(function(m){return m.right}(u)),i=function(m){return m.left}(u);return Ge(o)([yn(o)(I(O(c))(K(S(o))(Y(fc)(qt.value)("cursor: pointer;")))(Ms(k)(Xe)(S(o))(t)(function(m){return n(jt.create(m))})(function(m){return Ct(m)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(m){return function(s){return function(){var v=cr(z.value)(),D=Zi(m)(),l=Rf([nR(ys)(ge)(s)(function(nt){return function(){return En(new N(nt))(v)(),En(z.value)(v)}})])(Bf),g=xt(I(O(c))(p(k)(jt.create)(l))(p(k)(Jt.create)(tf)))(function(nt){if(nt instanceof jt)return nt.value0(D);if(nt instanceof Jt)return function(){var me=Mr(v)();return na(c)(Ve)(me)(function(Qt){return function(){var vr=O_(Qt)(),_t=Is(Os)(vr)(),ve=cr(0)(),Te=cr(0)(),re=cr(0)(),Wr=cr(0)(),Ao=cr(0)(),Bu=cr(0)(),Fi=cr(0)(),ru=cr(0)(),cn=cr(0)(),Jo=cr(0)(),qf=function(xr){if(xr<32)return ve;if(xr<64)return Te;if(xr<96)return re;if(xr<128)return Wr;if(xr<168)return Ao;if(xr<160)return Bu;if(xr<224)return Fi;if(tr)return ru;throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Analyser (line 143, column 45 - line 151, column 63): "+[xr.constructor.name])};sl(_t)(function(xr){var jo=Rs(xr);return function(){var Xo=Mr(Jo)();return Df(Le(Su)(jo))(cn)(),Df(Le(Su)(jo))(qf(Xo))(),Df(Le(Su)(1))(Jo)()}})();var Hf=jn(Cg)(c)(function(xr){return function(){var zf=p(x)(qe)(Mr(xr))(),Xo=p(x)(zu(pl)(zf))(p(x)(qe)(Mr(cn)))();return we(Me(ts)()(Wa)()(c_))(Xo>uR)(we(Me(Kn)()(sn)()(Wa))(Xo>oR)(we(Me(du)()(mn)()(sn))(Xo>iR)(we(Me(bu)()(vn)()(mn))(Xo>fR)(we(Me(Au)()(yu)()(vn))(Xo>cR)(Ru)))))}})(we(Me(Ag)()(Qv)()(gg))(ve)(we(Me(yg)()(Kv)()(Qv))(Te)(we(Me(kg)()(c_)()(Kv))(re)(we(Me(ts)()(Wa)()(c_))(Wr)(we(Me(Kn)()(sn)()(Wa))(Ao)(we(Me(du)()(mn)()(sn))(Bu)(we(Me(bu)()(vn)()(mn))(Fi)(we(Me(Au)()(yu)()(vn))(ru)(Ru)))))))))();return n(new Jt(Hf))()}})()};throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Analyser (line 124, column 57 - line 162, column 57): "+[nt.constructor.name])})();return function(){return g(),function(){var me=R_(ur)(m)();return Mn(c)(me!=="closed")(kn(ur)(m))()}(),n(new Jt(rs(Va)(T(rs(Ea)(T(!1))))))()}}}})(r)(f)))([nn(ae)(p(k)(function(m){if(m instanceof gu)return"Turn on";if(m instanceof qo)return"Loading...";if(m instanceof Ho)return"Turn off";throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Analyser (line 175, column 31 - line 178, column 56): "+[m.constructor.name])})(f))]),Se(o)(K(S(o))(Y(pt)(qt.value)("display: grid; grid-template-columns: repeat(8, 1fr); grid-auto-rows: 20px;")))([Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Tr)(_a)(Tr)(_o)(pt)(_a)(_o)(Za)(Za)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Tr)(_a)(Wn)(lo)(pt)(_a)(lo)(Ya)(Za)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Tr)(_a)(Ln)(co)(pt)(_a)(co)(Ka)(Za)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Tr)(_a)(Nn)(fo)(pt)(_a)(fo)(Qa)(Za)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Tr)(_a)(Rn)(io)(pt)(_a)(io)(Xa)(Za)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Tr)(_a)(Ea)(oo)(pt)(_a)(oo)(Hc)(Za)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Tr)(_a)(Lo)(uo)(pt)(_a)(uo)(qc)(Za)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Tr)(_a)(No)(ao)(pt)(_a)(ao)(Uc)(Za)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Wn)(la)(Tr)(_o)(pt)(la)(_o)(Za)(Ya)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Wn)(la)(Wn)(lo)(pt)(la)(lo)(Ya)(Ya)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Wn)(la)(Ln)(co)(pt)(la)(co)(Ka)(Ya)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Wn)(la)(Nn)(fo)(pt)(la)(fo)(Qa)(Ya)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Wn)(la)(Rn)(io)(pt)(la)(io)(Xa)(Ya)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Wn)(la)(Ea)(oo)(pt)(la)(oo)(Hc)(Ya)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Wn)(la)(Lo)(uo)(pt)(la)(uo)(qc)(Ya)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Wn)(la)(No)(ao)(pt)(la)(ao)(Uc)(Ya)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Ln)(ca)(Tr)(_o)(pt)(ca)(_o)(Za)(Ka)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Ln)(ca)(Wn)(lo)(pt)(ca)(lo)(Ya)(Ka)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Ln)(ca)(Ln)(co)(pt)(ca)(co)(Ka)(Ka)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Ln)(ca)(Nn)(fo)(pt)(ca)(fo)(Qa)(Ka)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Ln)(ca)(Rn)(io)(pt)(ca)(io)(Xa)(Ka)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Ln)(ca)(Ea)(oo)(pt)(ca)(oo)(Hc)(Ka)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Ln)(ca)(Lo)(uo)(pt)(ca)(uo)(qc)(Ka)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Ln)(ca)(No)(ao)(pt)(ca)(ao)(Uc)(Ka)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Nn)(fa)(Tr)(_o)(pt)(fa)(_o)(Za)(Qa)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Nn)(fa)(Wn)(lo)(pt)(fa)(lo)(Ya)(Qa)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Nn)(fa)(Ln)(co)(pt)(fa)(co)(Ka)(Qa)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Nn)(fa)(Nn)(fo)(pt)(fa)(fo)(Qa)(Qa)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Nn)(fa)(Rn)(io)(pt)(fa)(io)(Xa)(Qa)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Nn)(fa)(Ea)(oo)(pt)(fa)(oo)(Hc)(Qa)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Nn)(fa)(Lo)(uo)(pt)(fa)(uo)(qc)(Qa)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Nn)(fa)(No)(ao)(pt)(fa)(ao)(Uc)(Qa)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Rn)(ia)(Tr)(_o)(pt)(ia)(_o)(Za)(Xa)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Rn)(ia)(Wn)(lo)(pt)(ia)(lo)(Ya)(Xa)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Rn)(ia)(Ln)(co)(pt)(ia)(co)(Ka)(Xa)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Rn)(ia)(Nn)(fo)(pt)(ia)(fo)(Qa)(Xa)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Rn)(ia)(Rn)(io)(pt)(ia)(io)(Xa)(Xa)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Rn)(ia)(Ea)(oo)(pt)(ia)(oo)(Hc)(Xa)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Rn)(ia)(Lo)(uo)(pt)(ia)(uo)(qc)(Xa)(i)))([]),Se(o)(I(O(c))(K(S(o))(Y(pt)(qt.value)(Ie)))(Ne(k)(Rn)(ia)(No)(ao)(pt)(ia)(ao)(Uc)(Xa)(i)))([])])])}})))})}}};var _R=function(){return d.value}(),fS=function(t){return function(e){return function(r){return _e({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(o)(W()(Q(c))({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}})(o))(d.value)(_R)({bandpass:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([Sa(ie(ge)(a)(lt()))(function(u){return function(f){return Pt(ot)(.8)([An(on(kt(yt()(J(J(At)(bn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:400,q:1})([u]),An(on(kt(yt()(J(J(At)(bn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:880,q:5})([u]),An(on(kt(yt()(J(J(At)(bn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:1200,q:10})([u]),An(on(kt(yt()(J(J(At)(bn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:2e3,q:20})([u]),An(on(kt(yt()(J(J(At)(bn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:3e3,q:30})([u])])}})])}})))})}}};var sR=function(){return d.value}(),cS=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
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
`}})()()(W()(Q(c))({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}})(o))(o)(sR)({compression:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([vE(eE(kt(yt()(At))(bt()())))({})([ie(ge)(a)(lt())])])}})))})}}};var Zn=function(){return function(t){var e=en(),r=_r()({reflectSymbol:function(){return"playbackRate"}})(d.value),n=Pf(t);return function(a){return e(r(n(a)))}}},Uf=function(){return function(t){var e=en(),r=_r()({reflectSymbol:function(){return"onOff"}})(d.value),n=lC(t);return function(a){return e(r(n(a)))}}},lS=function(){return function(t){var e=en(),r=_r()({reflectSymbol:function(){return"offset"}})(d.value),n=Pf(t);return function(a){return e(r(n(a)))}}},_S=function(){var t=en(),e=_r()({reflectSymbol:function(){return"loopStart"}})(d.value);return function(r){return t(e(r))}},pS=function(){var t=en(),e=_r()({reflectSymbol:function(){return"loopEnd"}})(d.value);return function(r){return t(e(r))}},gn=function(){return function(t){var e=en(),r=_r()({reflectSymbol:function(){return"gain"}})(d.value),n=Pf(t);return function(a){return e(r(n(a)))}}},vo=function(){return function(t){var e=en(),r=_r()({reflectSymbol:function(){return"frequency"}})(d.value),n=Pf(t);return function(a){return e(r(n(a)))}}};var Vc=function(){return function(t){var e=en(),r=_r()({reflectSymbol:function(){return"delayTime"}})(d.value),n=Pf(t);return function(a){return e(r(n(a)))}}};var vR=function(){return d.value}(),sS=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
  <h2 id="constant">Constant value</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode">Constant values</a>, or DC offset, is a way to output an unchanging stream of values. This is only really useful when testing the performance of speakers or microphones and/or when working with a custom audio node that supports constant streaming values. Note that the constant source node in the web audio API can <i>also</i> be used to control audio parameters. Wags uses this feature of constant nodes under the hood to optimize certain computations.</p>

  <p>The following example abuses a constant audio node by turning it into a gnarly inpulse generator. We'll learn about the tie fighter symbol <code>~tf~</code> and the <code>bang</code> in the next section on Events. Kids, don't try this at home!</p>

  <pre><code>~txt~</code></pre>

  ~constant~
  </section>
`}})()()(W()(W()(W()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"tf"}})({reflectSymbol:function(){return"tf"}})(o))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}})(o))(o)(vR)({tf:L(ar(ae)("<|>")),txt:L(ar(ae)(`run2_
  [ gain_ 0.5
      [ constant 0.0
          ( bangOn <|>
              ( bang $ offset $ AudioEnvelope
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
  ]`)),constant:L(et(St(r)(t)(function(n){return h(sa)(void 0)})(function(n){return function(a){return vt(n)([Pt(ot)(.5)([Ss(vs)(0)(I(O(c))(lt())(K(S(o))(lS()(Tn)({d:5,o:.1,p:To(Wi)(function(u){return T(function(){var f=uu(Eo)(u)(3)===0;return f?1:0}())})(ln(0)(1920))}))))])])}})))})}}};var dR=function(){return d.value}(),mS=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
  <h2 id="convolution">Convolution</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConvolverNode">Convolution</a>, aka reverb, is a way to graft the shape of one sound (usually an <a href="https://en.wikipedia.org/wiki/Impulse_response">impulse response</a>) onto another. Convolution can sound great, but it is a <i>very expensive operation</i> that will cause noticeable artifacts on low-end devices. When shipping audio code to production, you're usually better off using an Audio Worklet Node with reverb optimized for your specific case. That said, for PoCs or hobbyist projects, convolution is great!</p>

  <pre><code>\\{loop, verb} -> run2_
  [ convolver verb [ loopBuf loop bangOn ] ]</code></pre>

  ~convolution~
  </section>
`}})()()(W()(Q(c))({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}})(o))(o)(dR)({convolution:L(et(St(r)(t)(function(n){return Gt(RE)(p(bi)(function(a){return function(u){return{loop:a,verb:u}}})(Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")))(Ct(n)("https://cdn.jsdelivr.net/gh/andibrae/Reverb.js/Library/StMarysAbbeyReconstructionPhase3.m4a"))})(function(n){return function(a){return vt(n)([lE(EC)(a.verb)([ie(ge)(a.loop)(lt())])])}})))})}}};var AR=function(){return d.value}(),vS=function(t){return function(e){return function(r){return _e({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(o)(W()(Q(c))({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}})(o))(d.value)(AR)({delay:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return vt(n)([Sa(Gn(Ba)(a)(lt()))(function(u){return function(f){return Pt(ot)(.2)([po(Qr)(.03)([u]),po(Qr)(.1)([u]),po(Qr)(.3)([u]),po(Qr)(.7)([u])])}})])}})))})}}};var kR=function(){return d.value}(),DS=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
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
`}})()()(W()(Q(c))({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}})(o))(o)(kR)({gain:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return vt(n)([Pt(ot)(.1)([ie(ge)(a)(lt())])])}})))})}}};var CR=function(){return d.value}(),dS=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
  <h2 id="highpass">Highpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highpass filter</a> lets higher frequencies pass and amortizes lower ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ highpass_ 2000.0
      [ loopBuf buf bangOn ]
  ]
</code></pre>

  ~highpass~
  </section>
`}})()()(W()(Q(c))({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}})(o))(o)(CR)({highpass:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([wc(Ja)(2e3)([ie(ge)(a)(lt())])])}})))})}}};var SR=function(){return d.value}(),bS=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
  <h2 id="highshelf">Highshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highshelf filter</a> boosts or attenuates high frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
  [ highshelf_ { frequency: 2000.0, gain: 0. }
      [ loopBuf buf bangOn ]
  ]</code></pre>

  ~highshelf~
  </section>
`}})()()(W()(Q(c))({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}})(o))(o)(SR)({highshelf:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([DE(tE(kt(yt()(J(J(At)(MC)()()()({reflectSymbol:function(){return"gain"}}))(wC)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:2e3,gain:.4})([ie(ge)(a)(lt())])])}})))})}}};var TR=function(){return d.value}(),AS=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
  <h2 id="iir">IIR filter</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/IIRFilterNode">IIR filter</a>, or infinite impulse response filter, is the Swiss Army Knife of filters. You can carve out and boost parts of the spectrum with amazing precision. But it comes with a catch: you can't automate the parameters. The parameters are also tough to work with if you're new to IIR filters. In short, you're setting up coefficients for a filter of type:</p>

  <pre><code>x0s0 + x1s1 + x2s2 + ... + y0S0 + y1S1 + y2S2 + ...</code></pre>

  <p>Where <code>s1</code> is the unfiltered signal at time <code>t-1</code>, <code>S0</code> is the <i>filtered</i> signal at time <code>t-1</code>, etc. The xs and ys are often called <i>feedforward</i> and <i>feedback</i> coefficients respectively.</p>

  <p>Because the Web Audio API accepts between 3 and 20 parameters for feedforward and feedback coefficients, Wags enforces that through a <a href="https://github.com/bodil/purescript-sized-vectors">sized vector</a>.</p>

  <pre><code>\\{loop, verb} -> run2_
  [ iirFilter
      ( (0.00020298 : 0.0004059599 : 0.00020298 : empty)
          /\\ (1.0126964558 : -1.9991880801 : 0.9873035442 : empty)
      )
      [ loopBuf buf bangOn ]
  ]</code></pre>
  ~iirFilterEx~
  </section>
`}})()()(W()(Q(c))({reflectType:function(){return"iirFilterEx"}})({reflectSymbol:function(){return"iirFilterEx"}})(o))(o)(TR)({iirFilterEx:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([_E()()(CC(pi)(pi))(new ut(Ef()()(20298e-8)(Ef()()(.0004059599)(Ef()()(20298e-8)(ov))),Ef()()(1.0126964558)(Ef()()(-1.9991880801)(Ef()()(.9873035442)(ov)))))([ie(ge)(a)(lt())])])}})))})}}};var FR=function(){return d.value}(),yS=function(t){return function(e){return function(r){return _e({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(o)(W()(Q(c))({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}})(o))(d.value)(FR)({loopBuf:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/100/100981_1234256-lq.mp3")})(function(n){return function(a){return vt(n)([ie(If(kt(yt()(J(J(J(J(At)(Fc)()()()({reflectSymbol:function(){return"playbackRate"}}))(A_)()()()({reflectSymbol:function(){return"loopStart"}}))(b_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Of)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())))({buffer:a,playbackRate:.5,loopStart:.1,loopEnd:.6})(lt()),ie(If(kt(yt()(J(J(J(J(At)(Fc)()()()({reflectSymbol:function(){return"playbackRate"}}))(A_)()()()({reflectSymbol:function(){return"loopStart"}}))(b_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Of)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())))({buffer:a,playbackRate:1,loopStart:.5,loopEnd:1.2})(lt()),ie(If(kt(yt()(J(J(At)(Fc)()()()({reflectSymbol:function(){return"playbackRate"}}))(Of)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())))({buffer:a,playbackRate:1.7})(lt())])}})))})}}};var MR=function(){return d.value}(),kS=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
  <h2 id="lowpass">Lowpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowpass filter</a> lets lower frequencies pass and amortizes higher ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ lowpass_ 215.0 [ loopBuf buf bangOn ] ]
</code></pre>

  ~lowpass~
  </section>
`}})()()(W()(Q(c))({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}})(o))(o)(MR)({lowpass:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([Pc(ds)(215)([ie(ge)(a)(lt())])])}})))})}}};var PR=function(){return d.value}(),gS=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
  <h2 id="lowshelf">Lowshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowshelf filter</a> boosts or attenuates lower frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
   [ lowshelf_ { frequency: 91.0, gain: 10.0 }
       [ loopBuf buf bangOn ]
   ]
</code></pre>

  ~lowshelf~
  </section>
`}})()()(W()(Q(c))({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}})(o))(o)(PR)({lowshelf:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([bE(ZC(kt(yt()(J(J(At)(xC)()()()({reflectSymbol:function(){return"gain"}}))(FC)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:91,gain:.4})([ie(ge)(a)(lt())])])}})))})}}};var IR=function(){return d.value}(),CS=function(t){return function(e){return function(r){return _e({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(o)(W()(Q(c))({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}})(o))(d.value)(IR)({microphone:L(et(St(r)(t)(function(n){return $s(!0)(!1)})(function(n){return function(a){return vt(n)([function(){if(a.microphone instanceof N)return ja(function(u){return Pt(ot)(1)([g_(D_)(a.microphone.value0),po(Qr)(.1)([Pt(ot)(.2)([u])])])});if(a.microphone instanceof z)return Pt(ot)(.02)([sE(Yi)(440)]);throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Microphone (line 43, column 15 - line 48, column 56): "+[a.microphone.constructor.name])}()])}})))})}}};var NR=function(){return d.value}(),ES=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
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
`}})()()(W()(Q(c))({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}})(o))(o)(NR)({notch:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([Oc(Mc(kt(yt()(J(J(At)(Tc)()()()({reflectSymbol:function(){return"q"}}))(xc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:400,q:1})(h(fr)(Oc(Mc(kt(yt()(J(J(At)(Tc)()()()({reflectSymbol:function(){return"q"}}))(xc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:880,q:5})(h(fr)(Oc(Mc(kt(yt()(J(J(At)(Tc)()()()({reflectSymbol:function(){return"q"}}))(xc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:1200,q:10})(h(fr)(Oc(Mc(kt(yt()(J(J(At)(Tc)()()()({reflectSymbol:function(){return"q"}}))(xc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:2e3,q:20})(h(fr)(Oc(Mc(kt(yt()(J(J(At)(Tc)()()()({reflectSymbol:function(){return"q"}}))(xc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:3e3,q:30})(h(fr)(ie(ge)(a)(lt())))))))))))])}})))})}}};var WR=function(){return d.value}(),SS=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
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
`}})()()(W()(Q(c))({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}})(o))(o)(WR)({peaking:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([Ic($c(kt(yt()(J(J(J(At)(Ec)()()()({reflectSymbol:function(){return"q"}}))(Sc)()()()({reflectSymbol:function(){return"gain"}}))(hc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:400,q:1,gain:-20})(h(fr)(Ic($c(kt(yt()(J(J(J(At)(Ec)()()()({reflectSymbol:function(){return"q"}}))(Sc)()()()({reflectSymbol:function(){return"gain"}}))(hc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:880,q:5,gain:20})(h(fr)(Ic($c(kt(yt()(J(J(J(At)(Ec)()()()({reflectSymbol:function(){return"q"}}))(Sc)()()()({reflectSymbol:function(){return"gain"}}))(hc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:1200,q:10,gain:-20})(h(fr)(Ic($c(kt(yt()(J(J(J(At)(Ec)()()()({reflectSymbol:function(){return"q"}}))(Sc)()()()({reflectSymbol:function(){return"gain"}}))(hc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:2e3,q:20,gain:20})(h(fr)(Ic($c(kt(yt()(J(J(J(At)(Ec)()()()({reflectSymbol:function(){return"q"}}))(Sc)()()()({reflectSymbol:function(){return"gain"}}))(hc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:3e3,q:30,gain:-20})(h(fr)(ie(ge)(a)(lt())))))))))))])}})))})}}};var GR=function(){return d.value}(),hS=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
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
`}})()()(W()(Q(c))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(o))(o)(GR)({periodic:L(et(St(r)(t)(function(n){return h(sa)(void 0)})(function(n){return function(a){return vt(n)([Pt(ot)(.2)([di(Di(kt(yt()(J(J(At)(vi(si(Kn)))()()()({reflectSymbol:function(){return"spec"}}))(mi)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:140,spec:new ut(we(Me(Kn)()(sn)()(Wa))(.1)(we(Me(du)()(mn)()(sn))(.2)(we(Me(bu)()(vn)()(mn))(.3)(we(Me(Au)()(yu)()(vn))(.4)(Ru)))),we(Me(Kn)()(sn)()(Wa))(.4)(we(Me(du)()(mn)()(sn))(.3)(we(Me(bu)()(vn)()(mn))(.2)(we(Me(Au)()(yu)()(vn))(.1)(Ru)))))})(lt())])])}})))})}}};var qR=function(){return d.value}(),TS=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
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
`}})()()(W()(Q(c))({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}})(o))(o)(qR)({playBuf:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/470/470035_9564355-lq.mp3")})(function(n){return function(a){return vt(n)([Gn(y_(kt(yt()(J(J(J(At)(hC)()()()({reflectSymbol:function(){return"duration"}}))(SC)()()()({reflectSymbol:function(){return"bufferOffset"}}))(d_)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())))({buffer:a,duration:3,bufferOffset:4.2})(lt())])}})))})}}};var fb=function(){function t(){}return t.value=new t,t}();var xS={attr:function(t){return function(e){return b({key:"controls",value:G(e)})}}};var cb=function(){function t(){}return t.value=new t,t}();var FS={attr:function(t){return function(e){return b({key:"src",value:G(e)})}}};var lb=function(t){return function(e){return function(r){return new w(V(t)("audio")(e)(B(r)))}}};var jR=function(t){return function(e){return function(r){return function(n){return LD(t)(n)(g_(e)(r))}}}},XR=function(){return d.value}(),$S=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
  <h2 id="recorder">Recorder</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioDestinationNode">recorder</a> allows you to record audio. It takes a callback that you can use to stash the recorded audio somewhere, like in a file for example, as the example below does. You can use it as a simple note-taking app \u{1F399}\uFE0F.</p>

  <pre><code>\\cb m -> recorder cb (microphone m)</code></pre>

  ~recorder~
  </section>
`}})()()(W()(Q(c))({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}})(o))(o)(XR)({recorder:L(et(jr(o)(function(n){return function(a){var u=$l(pu(c))(Z(tt))(a),f=$l(pu(c))(Z(tt))(function(_){return _.left}(u)),i=function(_){return _.right}(f),m=Bc(O(c))(S(o))(r)(function(_){return _.right}(u)),s=function(_){return _.left}(f);return Ge(o)([yn(o)(I(O(c))(K(S(o))(Y(fc)(qt.value)("cursor: pointer;")))(p(k)(function(_){return Y(sr)(lr.value)(Ke(T(function(){if(_.e instanceof qo)return h(c)(void 0);if(_.e instanceof Ho)return j(rt)(j(rt)(j(rt)(_.e.value0)(t(h(c)(void 0))))(na(c)(Ve)(_.rec)(function(){var v=t_(Ov);return function(D){return v(xs(D))}}())))(n(jt.create(gu.value)));if(_.e instanceof gu)return function(){_.cncl();var D=k_();n(new jt(qo.value))();var l=mo(U(Or)(p(bi)(function(g){return g.microphone})($s(!0)(!1)))(function(g){return mr(Pr)(function(){var st=Kt(h(c)(h(c)(void 0)))(function(me){return function(){var Je=Yn(ur)(),vr=Zi(Je)(),_t=Rf([jR(bD)(D_)(me)(function(Te){return function(){return n(new Jt(new jt(Te)))(),Ae(x)(As(Te)(D))(),XE("audio/ogg; codecs=opus")(function(Wr){return n(Jt.create(Jt.create(Wr)))})(Te)()}})])(Bf),ve=xt(_t)(function(Te){return Te(vr)})();return function(){ve(),U(Cn)(oE(D))(pr(c)(Ve)(function(){var Wr=t_(Ov);return function(Ao){return Wr(xs(Ao))}}()))();var re=R_(ur)(Je)();return Mn(c)(re!=="closed")(kn(ur)(Je))()}}})(g)();return n(new jt(new Ho(st)))(),st})}))();return t(function(){return n(jt.create(gu.value))(),Uo(yi(l))()})(),void 0};throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Recorder (line 63, column 47 - line 106, column 52): "+[_.e.constructor.name])}())))})(Sn(S(o))(I(O(c))(K(S(o))(z.value))(p(k)(N.create)(i)))(p(k)(ol)(Sn(S(o))(I(O(c))(K(S(o))(h(c)(void 0)))(p(k)(function(_){return _.value0})(r)))(p(k)(function(_){return function(v){return function(D){return{e:_,cncl:v,rec:D}}}})(m)))))))([nn(ae)(p(k)(function(_){if(_ instanceof gu)return"Turn on";if(_ instanceof qo)return"Loading...";if(_ instanceof Ho)return"Turn off";throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Recorder (line 117, column 31 - line 120, column 56): "+[_.constructor.name])})(m))]),Ge(o)([lb(o)(I(O(c))(K(S(o))(Y(xS)(fb.value)("true")))(I(O(c))(K(S(o))(Y(dv)(qt.value)("display:none;")))(I(O(c))(p(k)(function(_){return Y(FS)(cb.value)(_)})(s))(p(k)(T(Y(dv)(qt.value)("display:block;")))(s)))))([])])])}})))})}}};var KR=function(){return d.value}(),MS=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sawtoothOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(W()(Q(c))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(o))(o)(KR)({periodic:L(et(St(r)(t)(function(n){return h(sa)(void 0)})(function(n){return function(a){return vt(n)([Pt(ot)(.2)([pE(gC)(448)(lt())])])}})))})}}};var ZR=function(){return d.value}(),wS=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
  <h2 id="sine">Sine wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sine wave oscillator</a> plays back a sine wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sinOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(W()(Q(c))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(o))(o)(ZR)({periodic:L(et(St(r)(t)(function(n){return h(sa)(void 0)})(function(n){return function(a){return vt(n)([Pt(ot)(.2)([Nf(Yi)(448)(lt())])])}})))})}}};var eN=function(){return d.value}(),PS=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
  <h2 id="sawtooth">Square wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ squareOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(W()(Q(c))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(o))(o)(eN)({periodic:L(et(St(r)(t)(function(n){return h(sa)(void 0)})(function(n){return function(a){return vt(n)([Pt(ot)(.2)([C_(Cc)(448)(lt())])])}})))})}}};var nN=function(){return d.value}(),OS=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/StereoPannerNode">stereo panner</a> pans audio in the stereo plane. <code>-1.0</code> represents hard left, and <code>1.0</code> represents hard right, as in the example below.</p>

  <pre><code>\\buf -> run2_
  [ pan_ 1.0 [ loopBuf buf bangOn ] ]</code></pre>

  ~pan~
  </section>
`}})()()(W()(Q(c))({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}})(o))(o)(nN)({pan:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return vt(n)([AE(kC)(1)([ie(ge)(a)(lt())])])}})))})}}};var uN=function(){return d.value}(),IS=Ft({reflectType:function(){return`<ul>
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
`}})()()(Q(c))(o)(uN)({});var iN=function(){return d.value}(),RS=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ triangleOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(W()(Q(c))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(o))(o)(iN)({periodic:L(et(St(r)(t)(function(n){return h(sa)(void 0)})(function(n){return function(a){return vt(n)([Pt(ot)(.2)([Es(ms)(448)(lt())])])}})))})}}};var cN=function(){return d.value}(),NS=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
  <h2 id="waveshaper">Waveshaper</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/WaveshaperNode">waveshaper node</a>, aka distortion, uses a <a href="https://en.wikipedia.org/wiki/Waveshaper">waveshaping function</a> to add warmth to a sound.</p>

  <pre><code>~code~</code></pre>

  ~waveShaper~
  </section>
`}})()()(W()(W()(Q(c))({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}})(o))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}})(o))(o)(cN)({code:L(ar(ae)(`do
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
    [ waveShaper (makeFloatArray (makeDistortionCurve 400.0)) [ loopBuf buf bangOn ] ]`)),waveShaper:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){var u=function(f){var i=Xi/180;return p(xe)(function(m){var s=qe(m)*2/qe(44100)-1;return(3+f)*s*20*i/(Xi+f*nm($a)(lf)(s))})(ln(0)(44099))};return vt(n)([yE(YC)(Jd(u(400)))([ie(ge)(a)(lt())])])}})))})}}};var _N=function(){return d.value}(),LS=function(t){return function(e){return function(r){return function(n){var a=j(rt)(e(Wf.value))(fn),u=xa(t)(r);return Ft({reflectType:function(){return`<div>
  <h1>Audio Units</h1>

  <h3>There sure are a lot of them!</h3>
  <p>
    This section provides a tour of the web audio nodes provided by the Web Audio API and, by extension, Wags. There are only two omissions:</p>
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
</div>`}})()()(W()(W()(W()(W()(W()(W()(W()(W()(W()(W()(W()(W()(W()(W()(W()(W()(an()(W()(W()(W()(W()(W()(W()(W()(W()(W()(W()(W()(W()(Q(c))({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}})(o))({reflectType:function(){return"triangleOsc"}})({reflectSymbol:function(){return"triangleOsc"}})(o))({reflectType:function(){return"toc"}})({reflectSymbol:function(){return"toc"}})(o))({reflectType:function(){return"squareOsc"}})({reflectSymbol:function(){return"squareOsc"}})(o))({reflectType:function(){return"sinOsc"}})({reflectSymbol:function(){return"sinOsc"}})(o))({reflectType:function(){return"sawtoothOsc"}})({reflectSymbol:function(){return"sawtoothOsc"}})(o))({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}})(o))({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}})(o))({reflectType:function(){return"periodicOsc"}})({reflectSymbol:function(){return"periodicOsc"}})(o))({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}})(o))({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}})(o))({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}})(o))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}})(o))({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}})(o))({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}})(o))({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}})(o))({reflectType:function(){return"iirFilter"}})({reflectSymbol:function(){return"iirFilter"}})(o))({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}})(o))({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}})(o))({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}})(o))({reflectType:function(){return"drumroll"}})({reflectSymbol:function(){return"drumroll"}})(o))({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}})(o))({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}})(o))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}})(o))({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}})(o))({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}})(o))({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}})(o))({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}})(o))(o)(_N)({drumroll:L(et(ws("\u{1F941}")(n)(u)(function(f){return Ct(f)("https://freesound.org/data/previews/50/50711_179538-lq.mp3")})(function(f){return function(i){return vt(f)([Pt(ot)(1)([ie(ge)(i)(lt())])])}}))),toc:L(IS),allpass:L(QE(u)(e)(n)),analyser:L(iS(u)(e)(n)),bandpass:L(fS(u)(e)(n)),constant:L(sS(u)(e)(n)),compression:L(cS(u)(e)(n)),convolution:L(mS(u)(e)(n)),delay:L(vS(u)(e)(n)),gain:L(DS(u)(e)(n)),highpass:L(dS(u)(e)(n)),highshelf:L(bS(u)(e)(n)),iirFilter:L(AS(u)(e)(n)),loopBuf:L(yS(u)(e)(n)),lowshelf:L(gS(u)(e)(n)),lowpass:L(kS(u)(e)(n)),notch:L(ES(u)(e)(n)),playBuf:L(TS(u)(e)(n)),peaking:L(SS(u)(e)(n)),microphone:L(CS(u)(e)(n)),pan:L(OS(u)(e)(n)),periodicOsc:L(hS(u)(e)(n)),recorder:L($S(u)(e)(n)),sawtoothOsc:L(MS(u)(e)(n)),sinOsc:L(wS(u)(e)(n)),squareOsc:L(PS(u)(e)(n)),triangleOsc:L(RS(u)(e)(n)),waveShaper:L(NS(u)(e)(n)),next:Ta(O(c))(S(o))(n)(a)})}}}};var _b=function(){function t(){}return t.value=new t,t}(),WS={attr:function(t){return function(e){return b({key:"checked",value:G(e)})}}};var Do=function(){function t(){}return t.value=new t,t}();var zo={attr:function(t){return function(e){return b({key:"type",value:G(e)})}}};var bo=function(t){return function(e){return function(r){return new w(V(t)("input")(e)(B(r)))}}};var vN=function(t){return t},Gs=function(t){return function(e){return function(r){return Zu(t)(I(t.Plus0().Alt0())(K(t)(e))(r))}}};var W_=function(t){return function(e){return t(e)}},ef=function(t){return{map:function(e){return function(r){return function(n){return r(p(t)(function(a){return function(u){return a(e(u))}})(n))}}}}},Ci=function(t){return function(e){return function(r){return function(n){return W_(p(ef(t.Filterable1().Functor1()))(e)(r))(p(t.Filterable1().Functor1())(Vf)(n))}}}};var Jc=function(t){return Ci(t)(T)};var tu=vN;var BS=function(t){return function(e){return tu(function(r){return hr(S(o))(I(O(c))(K(S(o))(W_(t)(r)))(p(k)(function(n){return W_(n)(r)})(e)))})}},pb=function(t){return{apply:function(e){return function(r){return function(n){return r(e(p(t)(yo(Qo))(n)))}}},Functor0:function(){return ef(t)}}};var jc=function(t){return function(e){return Wt(function(r){return xt(e)(function(n){return function(){var u=I_(t)();return r({acTime:u,value:n})()}})})}};var GS=function(t){return function(e){return function(r){var n=function(a){return function(u){return function(f){return function(i){return function(m){return function(s){return function(){var v=Mr(f)();return Mn(c)(v)(function(){var l=I_(t)(),g=gp(Tk(Tu($a)(u-l-.04)(.01)*1e3))(function(){var st=Mr(f)();return Mn(c)(st)(function(){return En(u)(m)(),a(u)(),n(a)(u+s)(f)(i)(m)(s)()})()})();return En(new N(g))(i)()})()}}}}}}};return Wt(function(a){return function(){var f=cr(!0)(),i=cr(z.value)(),m=I_(t)(),s=cr(m+e)();n(a)(e)(f)(i)(s)(e)();var _=xt(r)(function(v){return function(){U(Cn)(Mr(i))(pr(c)(Ve)(Pl))();var l=Mr(s)();return n(a)(l+v)(f)(i)(s)(v)()}})();return j(rt)(j(rt)(_)(En(!1)(f)))(U(Cn)(Mr(i))(pr(c)(Ve)(Pl)))}})}}};var Fa=function(t){return function(e){return function(r){return function(n){return function(a){var u=r===t||n===e;if(u)return e;var f=(n-e)/(r-t),i=e-f*t;return f*a+i}}}}};var DN=function(){return d.value}(),US=function(t){return function(e){return function(r){return function(n){return Ft({reflectType:function(){return`<section>
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

</section>`}})()()(W()(W()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}})(o))(o)(DN)({txt:L(ar(ae)(`module Main where

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
import FRP.Event.Class (bang, fold, mapAccum, sampleOn)
import FRP.Event.VBus (V, vbus)
import Data.Number (pi, sin)
import Type.Proxy (Proxy(..))
import WAGS.Clock (withACTime)
import WAGS.Control (gain, periodicOsc)
import WAGS.Interpret (close, constant0Hack, context)
import WAGS.Math (calcSlope)
import WAGS.Core (AudioNumeric(..), _linear, bangOn)
import WAGS.Properties as P
import WAGS.Run (run2e)

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
        startE = bang unit <|> event.startStop.start
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
                            evs f a = sampleOn acTime
                              $ map ($)
                              $ sampleOn a
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
                        bang (D.Xtype := "checkbox")
                        bang (D.OnClick := cb (const (e unit)))
                        startE $> (D.Checked := "false")
                    )
                    []
                )
                ([ _.cbx0, _.cbx1, _.cbx2, _.cbx3 ] <@> push.cbx)
            )
        ]
  )`)),empl:L(et(su()(o)(wu({reflectSymbol:function(){return"cbx"}})()()()(nr({reflectSymbol:function(){return"cbx0"}})()()(nr({reflectSymbol:function(){return"cbx1"}})()()(nr({reflectSymbol:function(){return"cbx2"}})()()(nr({reflectSymbol:function(){return"cbx3"}})()()(In)()()()())()()()())()()()())()()()())(wu({reflectSymbol:function(){return"startStop"}})()()()(nr({reflectSymbol:function(){return"start"}})()()(nr({reflectSymbol:function(){return"stop"}})()()(In)()()()())()()()())(In)()()()())()()()())(d.value)(function(a){return function(u){var f=I(O(c))(K(S(o))(void 0))(u.startStop.start),i=function(D){return Gs(S(o))(!1)($u(S(o))(T(fu(Ia)))(D)(!1))},m=i(u.cbx.cbx3),s=i(u.cbx.cbx2),_=i(u.cbx.cbx1),v=i(u.cbx.cbx0);return Ge(o)([yn(o)(_n(Xt)(C(c))(p(k)(function(){var D=Y(sr)(lr.value);return function(l){return D(Ke(T(l)))}}()))([Ye(k)(Sn(S(o))(I(O(c))(K(S(o))(h(c)(void 0)))(p(k)(function(D){return D.value0})(n)))(X(k)(f)(Z(tt))))(function(D){return function(){D();var g=Yn(ur)(),nt=Wu(ur)(g)(),st=function(Je){return function(vr){return function(_t){return Ol(S(o))(function(ve){return function(Te){var re=Te.value1+(ve.value1-Te.value0)*function(){return ve.value0?Je:1}();return new ut(new ut(ve.value1,re),re)}})(Ci(S(o))(ut.create)(vr)(_t))(new ut(0,0))}}},me=Gc(g)(zi(o)(p(k)(function(){var Je=Le(Da)(.04);return function(vr){return Je(function(_t){return _t.acTime}(vr))}}())(jc(g)(tf)))(function(Je){var vr=function(Wr){return function(Ao){return Zu(S(o))(Je)(p(k)(ol)(Zu(S(o))(Ao)(p(k)(function(Bu){return function(Fi){return function(ru){return{f:Bu,a:Fi,t:ru}}}})(Wr))))}},_t=p(k)(function(Wr){return Wr?4:1})(Jc(S(o))(m)(Je)),ve=st(4)(s)(Je),Te=p(k)(function(Wr){return Wr?4:1})(Jc(S(o))(_)(Je)),re=st(8)(v)(Je);return[Lr(ot)(0)(Ye(k)(vr(re)(Te))(function(Wr){return gn()(Ir)({n:Fa(1)(.01)(4)(.15)(Wr.a)*Rp(Xi*Wr.f)+.15,o:Wr.t,t:Wo})}))([di(Di(kt(yt()(J(J(At)(vi(si(Kn)))()()()({reflectSymbol:function(){return"spec"}}))(mi)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:325.6,spec:new ut(we(Me(Kn)()(sn)()(Wa))(.3)(we(Me(du)()(mn)()(sn))(-.1)(we(Me(bu)()(vn)()(mn))(.7)(we(Me(Au)()(yu)()(vn))(-.4)(Ru)))),we(Me(Kn)()(sn)()(Wa))(.6)(we(Me(du)()(mn)()(sn))(.3)(we(Me(bu)()(vn)()(mn))(.2)(we(Me(Au)()(yu)()(vn))(0)(Ru)))))})(kr(Xt)(C(c))([lt(),Ye(k)(vr(ve)(_t))(function(Wr){return vo()(Ir)({n:325.6+Fa(1)(3)(4)(15.5)(Wr.a)*Rp(Xi*Wr.f),o:Wr.t,t:Wo})})]))])]}))(),Qt=j(rt)(j(rt)(me)(nt))(kn(ur)(g));return t(j(rt)(Qt)(a.startStop.start(void 0)))(),a.startStop.stop(Qt)()}}),Ye(k)(u.startStop.stop)(function(D){return j(rt)(D)(j(rt)(t(h(c)(void 0)))(a.startStop.start(void 0)))})]))([nn(ae)(kr(Xt)(C(c))([X(k)(f)("Turn on"),X(k)(u.startStop.stop)("Turn off")]))]),Se(o)(_n(Xt)(C(c))(p(k)(Y(pt)(qt.value)))([X(k)(u.startStop.stop)("display:block;"),X(k)(f)("display:none;")]))(p(xe)(function(D){return bo(o)(kr(Xt)(C(c))([K(S(o))(Y(zo)(Do.value)("checkbox")),K(S(o))(Y(sr)(lr.value)(Ke(T(D(void 0))))),X(k)(f)(Y(WS)(_b.value)("false"))]))([])})(am(xe)([function(D){return D.cbx0},function(D){return D.cbx1},function(D){return D.cbx2},function(D){return D.cbx3}])(a.cbx)))])}})))})}}}};var sb={recip:function(t){return 1/t},Ring0:function(){return lf}};var mb=function(t){return function(e){return{EuclideanRing0:function(){return t},DivisionRing1:function(){return e}}}};function Xc(t){return function(){return function(e){return t(e)()}}}function Qc(t){return function(e){return function(r){return function(n){return function(){return n.addEventListener(t,e,r)}}}}}function Kc(t){return function(e){return function(r){return function(n){return function(){return n.removeEventListener(t,e,r)}}}}}function vb(t){return t.clientX}function Db(t){return t.clientY}function B_(t){return t.button}var G_=It("MouseEvent");var qS=function(t){return function(e){return Wt(function(r){return xt(e)(function(n){return function(){var u=Mr(t.buttons)();return r({value:n,buttons:u})()}})})}};var HS=function(){var e=cr(z.value)(),r=cr(Xm)(),n=p(x)(ib)(gi)(),a=Xc(function(m){return pr(c)(Ve)(function(s){return En(new N({x:vb(s),y:Db(s)}))(e)})(G_(m))})(),u=Xc(function(m){return pr(c)(Ve)(function(s){return Xf(wy(qr)(B_(s)))(r)})(G_(m))})(),f=Xc(function(m){return pr(c)(Ve)(function(s){return Xf(kp(qr)(B_(s)))(r)})(G_(m))})();Qc(en()("mousemove"))(a)(!1)(n)(),Qc(en()("mousedown"))(u)(!1)(n)(),Qc(en()("mouseup"))(f)(!1)(n)();var i=function(){return Kc(en()("mousemove"))(a)(!1)(n)(),Kc(en()("mousedown"))(u)(!1)(n)(),Kc(en()("mouseup"))(f)(!1)(n)()};return{position:e,buttons:r,dispose:i}},zS=Wt(function(t){return function(){var r=p(x)(ib)(gi)(),n=Xc(function(a){return pr(c)(Ve)(function(u){return t(B_(u))})(G_(a))})();return Qc(en()("mousedown"))(n)(!1)(r)(),Kc(en()("mousedown"))(n)(!1)(r)}});var JS=function(t){return tu(function(e){return p(k)(function(r){return r.value(r.buttons)})(qS(t)(e))})};var Ab=function(t){return t};function Hs(){return Date.now()}var b0=function(t){return Wt(function(e){return xt(t)(function(r){return function(){var a=Hs();return e({time:a,value:r})()}})})};var YN=tu(function(t){return p(k)(function(e){return e.value(e.time)})(b0(t))}),kb=p(ef(k))(function(){var t=T_(OE);return function(e){return t(Ab(e))}}())(YN);var t1=function(t){var e=function(u){return function(f){return function(i){return function(m){return function(s){return function(_){return function(v){var D=Le(f.DivisionRing1().Ring0().Semiring0())(da(f.DivisionRing1().Ring0().Semiring0()))(da(f.DivisionRing1().Ring0().Semiring0())),l=function(g){return function(nt){if(g.last instanceof z)return nt;if(g.last instanceof N)return Le(i)(nt)(m(function(st){return zu(f.EuclideanRing0())($n(f.DivisionRing1().Ring0().Semiring0())(st(Le(i)(g.last.value0.value1)(g.now.value1)))(hu(f.DivisionRing1().Ring0())(g.now.value0)(g.last.value0.value0)))(D)}));throw new Error("Failed pattern match at WAGS.Example.Docs.FixEx (line 102, column 5 - line 102, column 35): "+[g.constructor.name,nt.constructor.name])}};return tu(function(g){var nt=W_(v)(X(u.Filterable1().Functor1())(g)(Z(tt))),st=Ep(u)(Ci(u)(ut.create)(_)(nt)),me=$u(u)(l)(st)(s);return Zu(u)(me)(g)})}}}}}}},r=function(u){return function(f){return e(u)(f)(f.DivisionRing1().Ring0().Semiring0())(function(i){return i(Z(tt))})}},n=function(u){return function(f){return tu(function(i){return Il(S(o))(function(m){var s=f(Gs(S(o))(u)(m));return{input:Jc(S(o))(s)(i),output:Zu(S(o))(m)(i)}})})}},a=function(u){return function(f){return function(i){if(My(u))return-8*(f-1)-i*2;if(tr)return 2*(4-f);throw new Error("Failed pattern match at WAGS.Example.Docs.FixEx (line 62, column 3 - line 64, column 34): "+[u.constructor.name,f.constructor.name,i.constructor.name])}}};return n(2)(function(u){return r(S(o))(mb(pl)(sb))(2)(p(ef(k))(Ar())(kb))(function(){var f=n(10)(function(i){return r(S(o))(mb(pl)(sb))(10)(p(ef(k))(Ar())(kb))(Gt(pb(k))(Gt(pb(k))(p(ef(k))(a)(JS(t)))(u))(i))});return BS(f)(X(k)(zS)(f))}())})},e1=function(){return d.value}(),A0=function(t){return function(e){return function(r){return function(n){return Ft({reflectType:function(){return`<section>
  <h2>Fix</h2>

  <p>Fix, like it's equivalent in wags that we've already seen, creates a feedback loop. However, in this case, we are talking about a feedback loop of <i>events</i>, not sound.</p>

  <p>At first glance, it may not be clear why we need an event stream to feed back into itself? It seems prone to saturation: if you have a counter that feeds back into itself with a delay, after a few seconds you'll have so many events that it will crash your browser (I've tried it!).</p>

  <p>However, there's one important circumstance where you need fixed points: when an event can only be defined in terms of itself. One classic category of this is the <i>differential equation</i>. Differential equations allow you to produce <a href="https://en.wikipedia.org/wiki/Simple_harmonic_motion">Slinky effects, aka simple harmonic motion,</a> and a lot of other neat behaviors that are difficult to produce via other means.</p>

  <p>Let's listen to the sound of simple harmonic motion in the example below, courtesy of <code>fix</code>. The differential equation in the example below comes from Phil Freeman, the creator of the PureScript language and the author of the <code>purescript-behaviors</code> package. When you click "Turn on", you won't hear much, but press and release your mouse anywhere on the screen to hear the differential equation take flight!</p>

  <pre><code>~txt~</code></pre>

  ~empl~

  <p>When working with stateful events, a good way to decide if you should use <code>fold</code> versus <code>fix</code> is to ask the following question: can I incrementally change my state based on an initial state, or is my state defined in terms of how it changes? If you can incrementally change your state, go with <code>fold</code>. If, on the other hand, your state is defined in terms of how it changes, go with <code>fix</code>.</p>
</section>`}})()()(W()(W()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}})(o))(o)(e1)({txt:L(ar(ae)(`module Main

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
import FRP.Event.Class (class IsEvent, bang, fix, fold, sampleOn, withLast)
import FRP.Event.Mouse (Mouse, down, getMouse)
import FRP.Event.VBus (V, vbus)
import Test.QuickCheck (arbitrary, mkSeed)
import Test.QuickCheck.Gen (evalGen)
import Type.Proxy (Proxy(..))
import WAGS.Clock (withACTime)
import WAGS.Control (bandpass_, gain, lowpass_, periodicOsc, squareOsc_)
import WAGS.Interpret (close, constant0Hack, context)
import WAGS.Core (AudioNumeric(..), _linear, bangOn)
import WAGS.Properties as P
import WAGS.Run (run2e)

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
        { input: sample_ b s, output: sampleOn event s }

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
        sampleOn z e
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
        startE = bang unit <|> event.start
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
  )`)),empl:L(et(su()(o)(nr({reflectSymbol:function(){return"start"}})()()(nr({reflectSymbol:function(){return"stop"}})()()(In)()()()())()()()())(d.value)(function(a){return function(u){var f=I(O(c))(K(S(o))(void 0))(u.start);return Ge(o)([yn(o)(_n(Xt)(C(c))(p(k)(function(){var i=Y(sr)(lr.value);return function(m){return i(Ke(T(m)))}}()))([Ye(k)(Sn(S(o))(I(O(c))(K(S(o))(h(c)(void 0)))(p(k)(function(i){return i.value0})(n)))(X(k)(f)(Z(tt))))(function(i){return function(){i();var s=Yn(ur)(),_=Wu(ur)(s)(),v=HS(),D=Xl(0)(1e4)(),l=function(_t){return{o:_t.value0+.04,n:_t.value1,t:Wo}},g=p(no)(function(_t){return _t-.5})(i_(ng)),nt=U(xf)(g)(function(_t){return U(xf)(g)(function(ve){return U(xf)(g)(function(Te){return U(xf)(g)(function(re){return h(u_)(we(Me(Kn)()(sn)()(Wa))(_t)(we(Me(du)()(mn)()(sn))(ve)(we(Me(bu)()(vn)()(mn))(Te)(we(Me(Au)()(yu)()(vn))(re)(Ru)))))})})})}),st=Gt(Ff)(p(no)(ut.create)(nt))(nt),me=Gt(Ff)(Gt(Ff)(Gt(Ff)(p(no)(function(_t){return function(ve){return function(Te){return function(re){return{s0:_t,s1:ve,s2:Te,s3:re}}}}})(st))(st))(st))(st),Qt=dc(me)({newSeed:sc(D),size:5}),Je=Gc(s)(zi(o)(p(k)(function(_t){return new ut(_t.acTime,_t.value)})(jc(s)(Jc(S(o))(t1(v))(tf))))(function(_t){return[Lr(ot)(0)(p(k)(function(){var ve=gn()(Ir),Te=Bn(Dn)(function(re){return Tu($a)(-.4)(.5*(re-1))});return function(re){return ve(l(Te(re)))}}())(_t))([Pc(MD(kt(yt()(J(J(At)($C)()()()({reflectSymbol:function(){return"q"}}))(AD)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:90.4,q:20})([mE(Cc)(90.4)])]),Lr(ot)(0)(p(k)(function(){var ve=gn()(Ir),Te=Bn(Dn)(function(re){return Tu($a)(-.2)(.4*(re-3))});return function(re){return ve(l(Te(re)))}}())(_t))([An(on(kt(yt()(J(J(At)(bn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:90.4*4,q:20})([di(Di(kt(yt()(J(J(At)(vi(si(Kn)))()()()({reflectSymbol:function(){return"spec"}}))(mi)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:90.4*3.02,spec:Qt.s0})(I(O(c))(lt())(p(k)(function(){var ve=vo()(Ir),Te=Bn(Dn)(function(re){return 90.4*3.02+14*(re-1)});return function(re){return ve(l(Te(re)))}}())(_t)))])]),Lr(ot)(0)(p(k)(function(){var ve=gn()(Ir),Te=Bn(Dn)(function(re){return Tu($a)(-.1)(.2*(re-6))});return function(re){return ve(l(Te(re)))}}())(_t))([An(on(kt(yt()(J(J(At)(bn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:90.4*6,q:20})([di(Di(kt(yt()(J(J(At)(vi(si(Kn)))()()()({reflectSymbol:function(){return"spec"}}))(mi)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:90.4*5.07,spec:Qt.s1})(I(O(c))(lt())(p(k)(function(){var ve=vo()(Ir),Te=Bn(Dn)(function(re){return 90.4*5.07+18*(re-1)});return function(re){return ve(l(Te(re)))}}())(_t)))])]),Lr(ot)(0)(p(k)(function(){var ve=gn()(Ir),Te=Bn(Dn)(function(re){return Tu($a)(0)(.2*(re-3))});return function(re){return ve(l(Te(re)))}}())(_t))([An(on(kt(yt()(J(J(At)(bn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:90.4*8,q:20})([di(Di(kt(yt()(J(J(At)(vi(si(Kn)))()()()({reflectSymbol:function(){return"spec"}}))(mi)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:90.4*7.13,spec:Qt.s2})(I(O(c))(lt())(p(k)(function(){var ve=vo()(Ir),Te=Bn(Dn)(function(re){return 90.4*7.13+32*(re-1)});return function(re){return ve(l(Te(re)))}}())(_t)))])]),Lr(ot)(0)(p(k)(function(){var ve=gn()(Ir),Te=Bn(Dn)(function(re){return Tu($a)(0)(.1*(re-7))});return function(re){return ve(l(Te(re)))}}())(_t))([di(Di(kt(yt()(J(J(At)(vi(si(Kn)))()()()({reflectSymbol:function(){return"spec"}}))(mi)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:90.4*9.14,spec:Qt.s3})(I(O(c))(lt())(p(k)(function(){var ve=vo()(Ir),Te=Bn(Dn)(function(re){return 90.4*9.14+31*(re-1)});return function(re){return ve(l(Te(re)))}}())(_t)))])]}))(),vr=j(rt)(j(rt)(Je)(_))(kn(ur)(s));return t(j(rt)(vr)(a.start(void 0)))(),a.stop(vr)()}}),Ye(k)(u.stop)(function(i){return j(rt)(i)(j(rt)(t(h(c)(void 0)))(a.start(void 0)))})]))([nn(ae)(kr(Xt)(C(c))([X(k)(f)("Turn on"),X(k)(u.stop)("Turn off")]))])])}})))})}}}};var n1=function(){return d.value}(),y0=function(t){return function(e){return function(r){return function(n){var a=xa(t)(r);return Ft({reflectType:function(){return`<div>
  <h1>State</h1>

  <h3>Or Events 2.0</h3>
  <p>
    The name of this section is a bit of a nisnomer. While it will address the issue of maintaining state in an audio graph, it's really just about two mechanisms you can use to make an <code>Event</code> stateful. One is called <code>fold</code>, and the other is called <code>fix</code>. Both are part of the <code>IsEvent</code> typeclass, which means you get them for free when working with events.
  </p>

  ~fold~
  ~fix~

  <h2>Next steps</h2>
  <p>Using <code>fold</code> and <code>fix</code>, we can create internal state in our Web Audio works that would be really tedious and error-prone to achieve in vanilla JS or other compile-to-JS languages. There's still one nagging issue that we haven't addressed, though. For all of the flexibility we can achieve with events, we still can't flex the audio graph itself, meaning that we can't add or remove components. In the next section, we'll learn how to do that with <a ~next~ style="cursor:pointer;">subgraphs</a>.</p>
</div>`}})()()(W()(W()(an()(Q(c))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))({reflectType:function(){return"fold"}})({reflectSymbol:function(){return"fold"}})(o))({reflectType:function(){return"fix"}})({reflectSymbol:function(){return"fix"}})(o))(o)(n1)({next:Ta(O(c))(S(o))(n)(j(rt)(e(P_.value))(fn)),fold:L(US(a)(e)(r)(n)),fix:L(A0(a)(e)(r)(n))})}}}};var u1=function(){function t(){}return t.value=new t,t}(),k0=function(){function t(){}return t.value=new t,t}(),gb=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),o1=`module Main where

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
import FRP.Event.Class (bang)
import Math (pow)
import WAGS.Control (gain_, gain, sinOsc)
import WAGS.Core (AudioEnvelope(..), AudioOnOff(..), _on, _off)
import WAGS.Properties (onOff)
import WAGS.Properties as P
import WAGS.Run (run2_)

data UIEvents = Init | Start | Stop (Effect Unit)

-- an event to turn our oscillators on
oon o = bang $ onOff $ AudioOnOff { x: _on, o }
-- an event to turn our oscillators off
oof o = bang $ onOff $ AudioOnOff { x: _off, o }
-- an event with an envelope for our gain
env o = bang $ P.gain
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
main = runInBody1 (bus \\push -> lcmap (bang Init <|> _) \\event ->
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
`;var i1=function(){return d.value}(),f1=function(t){return function(e){return function(r){return K(t)(Uf(e)(wf)({x:dD,o:r}))}}},c1=function(t){return function(e){return function(r){return K(t)(Uf(e)(wf)({x:bC,o:r}))}}},l1=Yu(ga)(qe)(function(t){var e=function(a){return I(O(c))(f1(S(o))()(a+.27*(t*ji(1.005)(t))))(c1(S(o))()(a+3+.3*(t*ji(1.005)(t))))},r=function(a){return K(S(o))(gn()(Tn)({p:[0,.4,.1,.05,.01,0],o:a+.3*(t*ji(1.005)(t)),d:.8}))},n=function(a){return function(u){return Lr(ot)(0)(r(a))([Nf(Yi)(200+t*u)(e(a))])}};return[n(.2)(4),n(.3)(6),n(.45)(14),n(.7)(20)]}),g0=function(t){return function(e){return function(r){return _e({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(o)(W()(W()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}})(o))(d.value)(i1)({txt:L(ar(ae)(o1)),ex0:L(et(jr(o)(function(n){return Yu(ga)(function(a){return I(O(c))(K(S(o))(u1.value))(a)})(function(a){return Ge(o)([yn(o)(Ye(k)(Sn(S(o))(I(O(c))(K(S(o))(h(c)(void 0)))(p(k)(function(u){return u.value0})(r)))(p(k)(ut.create)(a)))(function(u){return Y(sr)(lr.value)(Ke(T(function(){return u.value0 instanceof gb?j(rt)(j(rt)(u.value0.value0)(n(k0.value)))(t(h(c)(void 0))):function(){u.value1();var i=Ps([Pt(ot)(1)(Ha(Ko)(p(xe)(l1)(ln(0)(100))))])();return t(j(rt)(i)(n(k0.value)))(),n(new gb(i))()}}())))}))([nn(ae)(Ye(k)(a)(function(u){return u instanceof gb?"Turn off":"Turn on"}))])])})})))})}}};var Ei=function(){function t(){}return t.value=new t,t}();var nf={attr:function(t){return function(e){return b({key:"max",value:G(e)})}}};var Si=function(){function t(){}return t.value=new t,t}();var af={attr:function(t){return function(e){return b({key:"min",value:G(e)})}}};var hi=function(){function t(){}return t.value=new t,t}();var uf={attr:function(t){return function(e){return b({key:"input",value:ft(e)})}}};var Ti=function(){function t(){}return t.value=new t,t}(),of={attr:function(t){return function(e){return b({key:"step",value:G(e)})}}};var xi=function(){function t(){}return t.value=new t,t}();var ff={attr:function(t){return function(e){return b({key:"value",value:G(e)})}}};var Vo=function(t){return function(e){return function(r){return I(t)(e)(r(void 0))}}};var p1=Rk,eu={convert:function(t){return t}},U_={convert:function(t){return Yl(t)}},E0=function(t){return t},Cb=function(t){return t.convert},Ua=function(t){return function(e){return function(r){return gt(p1)(Yl(e))(Cb(t)(r(void 0)))}}};var q_=function(t){return function(e){return function(r){return function(n){return _n(Nk)(e)(r)(E0(Cb(t)(n)))}}}};function h0(t){return t.target}var Yc=function(t){return Xr(h0(t))};var v1=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (switcher, text, text_)
import Deku.Core (Domable, envy)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (create)
import FRP.Event.Class (bang, biSampleOn)
import FRP.Event.VBus (V, vbus)
import QualifiedDo.Alt as OneOf
import QualifiedDo.OneOfMap as O
import Type.Proxy (Proxy(..))
import WAGS.Control (loopBuf)
import WAGS.Core (bangOn)
import WAGS.Interpret (bracketCtx, decodeAudioDataFromUri)
import WAGS.Math (calcSlope)
import WAGS.Properties (loopEnd, loopStart, playbackRate)
import WAGS.Run (run2_)
import WAGS.WebAPI (BrowserAudioBuffer)
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
    :: forall lock payload
     . Maybe BrowserAudioBuffer
    -> Domable Effect lock payload
  scene = maybe (D.div_ [ text_ "Loading..." ]) \\buffer ->
    D.div_ $ pure $ envy $ vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        sl0 = event.slider.s0
        sl1 = event.slider.s1
        sl2 = event.slider.s2
        start = event.startStop.start <|> bang unit
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
                  (biSampleOn sl2 (add <$> (bang 0.0 <|> sl1)))
          ]
      D.div_
        $
          map
            ( \\{ l, f } -> D.div_
                [ text_ l
                , D.input
                    ( O.oneOfMap bang O.do
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
`,D1=function(){return d.value}(),d1="https://freesound.org/data/previews/100/100981_1234256-lq.mp3",T0=function(t){return function(e){return function(r){return _e({reflectType:function(){return`<section>
 <h2>Example 2: Three sliders</h2>

  <p>In this example, we'll use three sliders to control the playback rate, the start time, and the end time of a looping buffer.</p>

  <p>There is a fair bit of DOM-related code in this example, so before showing the whole thing, let's isolate the Wags bit.</p>

  <pre><code>@wagtxt@</code></pre>

  <p>Note that our loopBuf consumes four events: in addition to the three sliders, there is a <code>bangOn</code> event that turns it on. For the events belonging to range sliders, we use <code>calcSlope</code> to normalize the range to sensible values for these parameters.</p>

  <p>Because each slider event contains a number, we can compose it with a function from <code>WAGS.Properties</code>, like <code>playbackRate</code> or <code>loopStart</code>, to create an event that controls a Wags parameter. The <code>oneOf</code> directive indicates that the incoming event will be "one of" the events in the array. It's also possible to use the tie-fighter, aka <code>alt</code>, to separate each event, but I like the array syntax when possible as tie fighters do, after all, work for the Empire, and who likes the Empire?</p>

  <p>And below you'll find the full example. It also shows useful patterns like downloading audio files and filtering events.</p>

  <pre><code>@txt@</code></pre>

  @ex1@

</section>
`}})({reflectType:function(){return"@"}})()()(o)(W()(W()(W()(Q(c))({reflectType:function(){return"wagtxt"}})({reflectSymbol:function(){return"wagtxt"}})(o))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(o))(d.value)(D1)({wagtxt:L(ar(ae)(`run2_
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
          (add <$> (bang 0.0 <|> sl1))`)),txt:L(ar(ae)(v1)),ex1:L(et(su()(o)(wu({reflectSymbol:function(){return"slider"}})()()()(nr({reflectSymbol:function(){return"s0"}})()()(nr({reflectSymbol:function(){return"s1"}})()()(nr({reflectSymbol:function(){return"s2"}})()()(In)()()()())()()()())()()()())(wu({reflectSymbol:function(){return"startStop"}})()()()(nr({reflectSymbol:function(){return"loading"}})()()(nr({reflectSymbol:function(){return"start"}})()()(nr({reflectSymbol:function(){return"stop"}})()()(In)()()()())()()()())()()()())(In)()()()())()()()())(d.value)(function(n){return function(a){var u=I(O(c))(a.startStop.start)(K(S(o))(void 0)),f=function(i){return ie(If(kt(yt()(J(J(J(J(At)(Fc)()()()({reflectSymbol:function(){return"playbackRate"}}))(A_)()()()({reflectSymbol:function(){return"loopStart"}}))(b_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Of)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())))({buffer:i,playbackRate:2.6,loopStart:.6,loopEnd:1.1})(Vo(O(c))(lt())(function(){return Vo(O(c))(p(k)(function(){var m=Zn()(ss),s=Fa(0)(.2)(100)(5);return function(_){return m(s(_))}}())(a.slider.s0))(function(){return Vo(O(c))(p(k)(function(){var m=_S(),s=Fa(0)(0)(100)(1.2);return function(_){return m(s(_))}}())(a.slider.s1))(function(){return p(k)(function(){var m=pS(),s=Fa(0)(.05)(100)(1);return function(_){return m(s(_))}}())(Sn(S(o))(a.slider.s2)(p(k)(Le(Da))(I(O(c))(K(S(o))(0))(a.slider.s1))))})})}))};return Ge(o)(gt(ba)(p(xe)(function(i){return Ge(o)([ar(ae)(i.l),bo(o)(q_(eu)(C(c))(K(S(o)))(Ua(eu)(Y(zo)(Do.value)("range"))(function(){return Ua(eu)(Y(af)(Si.value)("0"))(function(){return Ua(eu)(Y(nf)(Ei.value)("100"))(function(){return Ua(eu)(Y(of)(Ti.value)("1"))(function(){return Ua(U_)(Y(ff)(xi.value)("50"))(function(){return Y(uf)(hi.value)(Ke(function(){var m=pr(c)(Ve)(Jf(Cn)(Gf)(i.f)),s=qn(ya)(zc);return function(_){return m(s(Yc(_)))}}()))})})})})})))([])])})([{l:"Playback rate",f:n.slider.s0},{l:"Loop start",f:n.slider.s1},{l:"Loop end",f:n.slider.s2}]))([yn(o)(q_(eu)(C(c))(p(k)(function(){var i=Y(sr)(lr.value);return function(m){return i(Ke(T(m)))}}()))(Ua(eu)(X(k)(a.startStop.loading)(h(c)(void 0)))(function(){return Ua(U_)(Ye(k)(a.startStop.stop)(function(i){return j(rt)(i)(j(rt)(t(h(c)(void 0)))(n.startStop.start(void 0)))}))(function(){return Ye(k)(Sn(S(o))(I(O(c))(K(S(o))(h(c)(void 0)))(p(k)(function(i){return i.value0})(r)))(X(k)(u)(Z(tt))))(function(i){return function(){i(),n.startStop.loading(void 0)();var s=mo(U(Or)(Yn(Pr))(function(_){return U(Or)(Wu(Pr)(_))(function(v){return U(Or)(Ct(_)(d1))(function(D){return mr(Pr)(function(){var g=vt(_)([f(D)])(),nt=j(rt)(j(rt)(g)(v))(kn(ur)(_));return n.startStop.stop(nt)(),nt})})})}))();return t(function(){return n.startStop.start(void 0)(),Uo(yi(s))()})(),void 0}})})})))([nn(ae)(Vo(O(c))(p(k)(T("Turn off"))(a.startStop.stop))(function(){return p(k)(T("Turn on"))(u)}))])]))}})))})}}};var A1=`module Main where

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
import FRP.Event.Class (bang)
import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import WAGS.Clock (interval)
import WAGS.Control (bandpass_, fan1, gain, gain_, highpass_, triangleOsc)
import WAGS.Core (Audible, AudioEnvelope(AudioEnvelope), bangOn)
import WAGS.Interpret (close, context)
import WAGS.Math (calcSlope)
import WAGS.Properties (frequency)
import WAGS.Properties as P
import WAGS.Run (run2e)
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
        start = event.startStop.start <|> bang unit

        music :: forall lock. _ -> Event (Array (Audible _ lock _))
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
                ( O.oneOfMap bang O.do
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
  )`,y1=tu(function(t){return Wt(function(e){return xt(t)(function(r){return function(){var a=Ou();return e(r(a))()}})})}),k1=function(){return d.value}(),g1=function(t){if(t<.142857)return 261.625565;if(t<.285714)return 293.664768;if(t<.428571)return 349.228231;if(t<.571429)return 391.995436;if(t<.714286)return 440;if(t<.857143)return 523.251131;if(tr)return 587.329536;throw new Error("Failed pattern match at WAGS.Example.Docs.Events.Ex2 (line 225, column 1 - line 225, column 23): "+[t.constructor.name])},x0=function(t){return function(e){return function(r){return _e({reflectType:function(){return`<section>
  <h2>Example 3: Fascinating rhyhtm</h2>

  <p>Wags comes with several different ways to hook into the Web Audio API's sample-accurate timers. In this section, we'll use a Wags <code>interval</code> event to create a sample-accurate ticker. We'll also use a <code>random</code> beahvior to change up our samples.</p>

  <p><code>interval :: AudioContext -> Event Number -> Event Number</code> in wags is similar to <a href=""><code>interval :: Int -> Event Instant</code></a> from the <code>Event</code> library with a few important exceptions.</p>

  <ul>
    <li>The wags interval works in seconds (<code>Number</code>) instead of milliseconds.</li>
    <li>The wags interval needs an audio context to work.</li>
    <li>The wags interval gets its timing from an <code>Event Number</code> instead of a plain old <code>Number</code>. This is necessary to have variable rates.</li>
  </ul>

  <blockquote><code>interval</code> works fine for a stream of events where each event is separated by more than ~100 milliseconds. For anything faster, you'll likely want to use <code>requestAnimationLoop</code> coupled with a local state, as it will be more efficient for older and battery-sensitive devices.</blockquote>

  <p>In the following example, we use <code>interval</code> to control the playback rate of an analogue synth. We'll also use a custom behavior called <code>random</code> to control the pitch.</p>

  <p>One important optimization we make here is the use of the function <code>memoize</code>. Whenever we're dealing with audio-clock timing, we want to limit the number of subscriptions to receive events from the audio clock. Ideally, there is only one subscription that takes a reading of the clock as a single source of truth. Because we are in PureScript-land, events (like everything else), are referrentially transparent, meaning that new ones will get created every time you use them (just like a new <code>2</code> is created every time you type the value <code>2</code>: they don't all refer to one uber-<code>2</code>). To sync all the events to the <i>same</i> source, we use <code>memoize</code>. While this optimization is not necessary, I recommend it: it will make sure the timing is 100% accurate at a very small energy cost (meaning <code>memoize</code> will eat up slightly more power from a phone's battery, but still not much).</p>

  <pre><code>@txt@</code></pre>

  @ex2@

</section>
`}})({reflectType:function(){return"@"}})()()(o)(W()(W()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}})(o))(d.value)(k1)({txt:L(ar(ae)(A1)),ex2:L(et(su()(o)(nr({reflectSymbol:function(){return"slider"}})()()(wu({reflectSymbol:function(){return"startStop"}})()()()(nr({reflectSymbol:function(){return"start"}})()()(nr({reflectSymbol:function(){return"stop"}})()()(In)()()()())()()()())(In)()()()())()()()())(d.value)(function(n){return function(a){var u=I(O(c))(a.startStop.start)(K(S(o))(void 0)),f=function(i){return zi(o)(i)(function(m){var s=p(k)(function(){var nt=Le(Da)(.01);return function(st){return nt(tn(st))}}())(m),_=p(k)(Na)(m),v=I(O(c))(lt())(p(k)(function(){var nt=vo()(ss);return function(st){return nt(g1(st))}}())(_)),D=p(k)(function(nt){return _s(function(st){return{p:[0,.15,.05,.01,.005,5e-4,0],d:.4,o:st}}(nt))})(s),l=p(k)(function(nt){return _s(function(st){return{p:[0,.3,.1,.05,.01,.005,0],d:.4,o:st}}(nt))})(s),g=p(k)(function(nt){return _s(function(st){return{p:[0,.6,.2,.1,.5,.03,0],d:.4,o:st}}(nt))})(s);return[Sa(Es(ms)(0)(v))(function(nt){return function(st){return Pt(ot)(2)([Lr(ot)(0)(p(k)(gn()(Tn))(g))([An(on(kt(yt()(J(J(At)(bn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:1e3,q:20})([nt])]),Lr(ot)(0)(p(k)(gn()(Tn))(l))([An(on(kt(yt()(J(J(At)(bn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:2e3,q:20})([nt])]),Lr(ot)(0)(p(k)(gn()(Tn))(D))([wc(wD(kt(yt()(J(J(At)(PC)()()()({reflectSymbol:function(){return"q"}}))(yD)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:4e3,q:20})([nt])])])}})]})};return Ge(o)([Ge(o)([ar(ae)("tempo"),bo(o)(q_(eu)(C(c))(K(S(o)))(Ua(eu)(Y(zo)(Do.value)("range"))(function(){return Ua(eu)(Y(af)(Si.value)("0"))(function(){return Ua(eu)(Y(nf)(Ei.value)("100"))(function(){return Ua(eu)(Y(of)(Ti.value)("1"))(function(){return Ua(U_)(Y(ff)(xi.value)("50"))(function(){return Y(uf)(hi.value)(Ke(function(){var i=pr(c)(Ve)(Jf(Cn)(Gf)(n.slider)),m=qn(ya)(zc);return function(s){return i(m(Yc(s)))}}()))})})})})})))([])]),yn(o)(_n(Xt)(C(c))(p(k)(function(){var i=Y(sr)(lr.value);return function(m){return i(Ke(T(m)))}}()))([Ye(k)(Sn(S(o))(I(O(c))(K(S(o))(h(c)(void 0)))(p(k)(function(i){return i.value0})(r)))(X(k)(u)(Z(tt))))(function(i){return function(){i();var s=Yn(ur)(),_=Ci(S(o))(ut.create)(y1)(GS(s)(.91)(p(k)(Fa(0)(.42)(100)(1.4))(a.slider))),v=Gc(s)(f(_))(),D=j(rt)(v)(kn(ur)(s));return t(j(rt)(D)(n.startStop.start(void 0)))(),n.startStop.stop(j(rt)(D)(kn(ur)(s)))()}}),Ye(k)(a.startStop.stop)(function(i){return j(rt)(i)(j(rt)(t(h(c)(void 0)))(n.startStop.start(void 0)))})]))([nn(ae)(kr(Xt)(C(c))([X(k)(u)("Turn on"),X(k)(a.startStop.stop)("Turn off")]))])])}})))})}}};var E1=function(){return d.value}(),F0=function(){return _e({reflectType:function(){return`<section>
  <h2>Three flavors of events.</h2>

  <p>When we're in the browser, events tend to come in three broad categories:</p>

  <ul>
    <li>Things that need to happen <span style="font-weight: 800;">now</span>.</li>
    <li>Things that happen as the result of a user interaction.</li>
    <li>Things that are scheduled to happen in the future, for example with <code>setTimeout</code>.</li>
  </ul>

  <p>The next three examples cover all three cases.</p>

</section>`}})({reflectType:function(){return"@"}})()()(o)(Q(c))(d.value)(E1)({})}();var h1=function(){return d.value}(),$0=function(){return _e({reflectType:function(){return`<section>
  <h2>Events in Wags</h2>
  <p>Wags follows a consistent pattern: every audio unit accepts an event containing a <code>newtype</code> around a <code>Variant</code> of parameters that can be changed. As a motivating example, let's look at the definition of <code>sinOsc</code>.</p>

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

  <p>In practice, you'll never need to use newtypes presented above. The <code>WAGS.Properties</code> module has highly-overloaded smart constructors for all these values. For example, to use an <code>Event Number</code> to set the frequency of a <code>sinOsc</code>, you'd write <code>frequency &lt;$&gt; event</code> instead of <code>SinOsc &lt;&lt;&lt; inj (Proxy :: _ "frequency") &lt;&lt;&lt; AudioParameter &lt;&lt;&lt; { o: 0.0, t: _linearRamp, n: _ } &lt;$&gt; event</code>. Of course, you <i>can</i> write the longer format, but life is short and keystrokes are precious! Below is a table showing the varoius smart constructors available and the units to which they apply.</p>


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

  <p>You can use this smart-constructor pattern to transform any <code>Event</code> into something that Wags can consume. For example:</p>

  <ul>
    <li>If you have an <code>Event Number</code> called <code>myFreq</code> and you'd like it to control the frequency of a band-pass filter, you can write <code>bandpass 440.0 (frequency &lt;$&gt; myFreq)</code>.</li>
    <li>If you have an <code>Event Number</code> called <code>myQ</code> and you'd like it to control the Q value of the same bandpass, you can write <code>bandpass 440.0 (frequency &lt;$&gt; myFreq &lt;|&gt; q &lt;$&gt; myQ)</code> <i>or</i> <code>bandpass 440.0 $ oneOf [frequency &lt;$&gt; myFreq, q &lt;$&gt; myQ]</code>.</li>
    <li>If you'd like <code>myFreq</code> <i>only</i> to have an effect when it's over <code>1000.0</code>, you can write <code>bandpass 440.0 (frequency &lt;$&gt; filter (_ > 1000.0) myFreq &lt;|&gt; q &lt;$&gt; myQ)</code>.</li>
  </ul>

  <p>None of these transformations are unique to Wags:</p>
  <ul>
    <li>Because <code>Event</code> implements <a href=""><code>Functor</code></a>, you can use <code>map</code> (aka <code>&lt;$&gt;</code> above).</li>
    <li>Because <code>Event</code> implements <a href=""><code>Alt</code></a>, you can use <code>alt</code> (aka <code>&lt;|&gt;</code> above).</li>
    <li>Because <code>Event</code> implements <a href=""><code>Plus</code></a>, you can use <code>empty</code> for an event that emits nothing as well as <code>oneOf</code>.</li>
    <li>Because <code>Event</code> implements <a href=""><code>Filterable</code></a>, you can use <code>filter</code>, <code>filterMap</code>, <code>partition</code>, <code>partitionMap</code>, and <code>compact</code>.</li>
    <li>Because <code>Event</code> implements <a href=""><code>IsEvent</code></a>, you can use <code>bang</code> to emit something right away, <code>sampleOn</code> to sample one event's most recent value based on another event, and all of the other functions rolled into <a href=""><code>IsEvent</code></a>.</li>
  </ul>

  <p>
    This gets to one of the core design principles of Wags. Idiomatic Wags projects use functional reactive programming as a way to "steer" web audio, and Wags aims to be a minimal viable framework to shepherd events to their web-audio destinations.
  </p>
</section>`}})({reflectType:function(){return"@"}})()()(o)(Q(c))(d.value)(h1)({})}();var x1=function(){return d.value}(),M0=function(){return _e({reflectType:function(){return`<section>

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
    When using Wags, you have to get your events from somewhere. At a minimum, you'll consume a browser interaction like a click or swipe that turns on the audio. In fact, without some form of human interaction, most browsers will block the Web Audio API from turning on.
  </p>
  <p>
    <code>Events</code> are often produced within a web framework like <a href="https://github.com/mikesol/purescript-deku">Deku</a>, Halogen or React. They don't have to be, though - you can create and consume your own events.
  </p>

  <h3>Behavior</h3>

  <p>
    The <code>Behavior</code> type takes an event that needs to be "unlocked" (meaning in the form of <code>a -> b</code>, so an <code>a</code> is needed to unlock a <code>b</code>) and unlocks it with an <code>a</code>. Behaviors don't need to produce their <code>a</code> immediately. In fact, they don't need to produce it at all: it's entirely possible to create <code>Behavior (const empty)</code> that "mutes" the event. This resembles the physical world: when we want to observe a behavior, like the weather outside or the axial rotation of the Earth, there is a time-cost to observing anything that ranges from instantaneous to infinite.
  </p>

  <p>
    In Wags, we usually want to observe the behavior of things like a mouse's position, an audio buffer's content or a random number generator.
  </p>
</section>`}})({reflectType:function(){return"@"}})()()(o)(Q(c))(d.value)(x1)({})}();var $1=function(){return d.value}(),w0=function(t){return function(e){return function(r){return function(n){var a=function(f){return Ta(O(c))(S(o))(n)(j(rt)(e(f))(fn))},u=xa(t)(r);return _e({reflectType:function(){return`<div>
  <h1>Events</h1>

  <h3>Clicks, wiggles and loops, oh my!</h3>
  <p>
    The true magic of web audio lies in its ability to harness the rich interactivity built into the browser. We can use mouse clicks, finger swipes and animation loops to create beautiful audio landscapes. But how can we tame the complexity of all these events in an expressive, declarative, functional manner? Enter <code>Event</code>, the abstraction that allows us to build rich reactive works using Wags.
  </p>

  @primer@
  @inWags@
  @flavors@
  @ex0@
  @ex1@
  @ex2@

  <h2>Next steps</h2>
  <p>In this section, saw how to build rich audio applications using the <code>Event</code> and <code>Behavior</code> types. We also covered the three most common patterns you'll see when working with events: events that need to happen <i>now</i>, events that come from user interaction, and timed events. In the next section, we'll look at different ways to specify <a @next@ style="cursor:pointer;">the numeric parameters being sent as events</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(o)(W()(W()(W()(W()(W()(an()(W()(Q(c))({reflectType:function(){return"primer"}})({reflectSymbol:function(){return"primer"}})(o))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))({reflectType:function(){return"inWags"}})({reflectSymbol:function(){return"inWags"}})(o))({reflectType:function(){return"flavors"}})({reflectSymbol:function(){return"flavors"}})(o))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}})(o))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(o))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}})(o))(d.value)($1)({next:a(M_.value),primer:L(M0),inWags:L($0),flavors:L(F0),ex0:L(g0(u)(e)(n)),ex1:L(T0(u)(e)(n)),ex2:L(x0(u)(e)(n))})}}}};var w1=function(){return d.value}(),P0=function(t){return function(e){return function(r){return _e({reflectType:function(){return`<div>
  <pre><code>\\{ tink0, tink1, tink2, tink3 } -> run2_
  [ gain_ 1.0 do
      let ooo n = bang $ onOff $ dt (add n) apOn
      [ playBuf tink0 (ooo 0.1)
      , playBuf tink1 (ooo 0.2)
      , playBuf tink2 (ooo 0.9)
      , playBuf tink3 (ooo 1.8)
      ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(o)(W()(Q(c))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(o))(d.value)(w1)({ai0:L(et(St(r)(t)(function(n){return so(Fn)(Gt(Ai)(Gt(Ai)(Gt(Ai)(p(Lc)(function(a){return function(u){return function(f){return function(i){return{tink0:a,tink1:u,tink2:f,tink3:i}}}}})(xn(Fn)(Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(xn(Fn)(Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(xn(Fn)(Ct(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(xn(Fn)(Ct(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(a){return vt(n)([Pt(ot)(1)(function(){var u=function(f){return K(S(o))(Uf()(wf)(ps()(Le(Da)(f))(v_)))};return[Gn(Ba)(a.tink0)(u(.1)),Gn(Ba)(a.tink1)(u(.2)),Gn(Ba)(a.tink2)(u(.9)),Gn(Ba)(a.tink3)(u(1.8))]}())])}})))})}}};var O1=function(){return d.value}(),O0=function(t){return function(e){return function(r){return _e({reflectType:function(){return`<div>
  <pre><code>\\{ tink0, tink1, tink2, tink3 } -> run2_
  [ gain_ 1.0
      $ do
          let
            ooo n = bang $ onOff $ dt (add n) apOn
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
`}})({reflectType:function(){return"@"}})()()(o)(W()(Q(c))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(o))(d.value)(O1)({ai0:L(et(St(r)(t)(function(n){return so(Fn)(Gt(Ai)(Gt(Ai)(Gt(Ai)(p(Lc)(function(a){return function(u){return function(f){return function(i){return{tink0:a,tink1:u,tink2:f,tink3:i}}}}})(xn(Fn)(Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(xn(Fn)(Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(xn(Fn)(Ct(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(xn(Fn)(Ct(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(a){return vt(n)([Pt(ot)(1)(function(){var u=function(i){return K(S(o))(Uf()(wf)(ps()(Le(Da)(i))(v_)))},f=function(i){var m=uu(Eo)(i)(4);return m===0?a.tink0:m===1?a.tink1:m===2?a.tink2:a.tink3};return Ye(xe)(ln(0)(100))(function(i){var m=qe(i);return Gn(Ba)(f(i))(u(.3+.3*(m*ji(1.005)(m))))})}())])}})))})}}};var R1=function(){return d.value}(),I0=function(t){return function(e){return function(r){return _e({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(o)(W()(Q(c))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(o))(d.value)(R1)({ai0:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([Sa(ie(ge)(a)(lt()))(function(u){return function(f){return Pt(ot)(.8)([An(on(kt(yt()(J(J(At)(bn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:400,q:1})([u]),An(on(kt(yt()(J(J(At)(bn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:880,q:5})([u]),An(on(kt(yt()(J(J(At)(bn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:1200,q:10})([u]),An(on(kt(yt()(J(J(At)(bn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:2e3,q:20})([u]),An(on(kt(yt()(J(J(At)(bn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:3e3,q:30})([u])])}})])}})))})}}};var L1=function(){return d.value}(),R0=function(t){return function(e){return function(r){return _e({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(o)(W()(Q(c))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(o))(d.value)(L1)({ai0:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([Sa(ie(ge)(a)(lt()))(function(u){return function(f){return Pt(ot)(.8)(Ye(xe)(ln(0)(40))(Yu(ga)(qe)(function(i){return An(on(kt(yt()(J(J(At)(bn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:200+i*150,q:30})([u])})))}})])}})))})}}};var B1=function(){return d.value}(),N0=function(t){return function(e){return function(r){return _e({reflectType:function(){return`<div>
  <pre><code>\\buf -> run2_
  [ fix
      \\b -> gain_ 1.0
        [ playBuf buf bangOn
        , delay_ 0.1 [ gain_ 0.6 [ b ] ]
        ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(o)(W()(Q(c))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(o))(d.value)(B1)({ai0:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(a){return vt(n)([ja(function(u){return Pt(ot)(1)([Gn(Ba)(a)(lt()),po(Qr)(.1)([Pt(ot)(.6)([u])])])})])}})))})}}};var U1=function(){return d.value}(),q1=function(t){return function(e){return K(t)(gn(e)(Tn)({p:[1,1,0],o:0,d:10}))}},H1=function(t){return function(e){return K(t)(gn(e)(Tn)({p:[1,1,0],o:0,d:8}))}},Zc=function(t){return function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return po(t)(n)([Pt(e)(a)([wc(r)(u)(f)])])}}}}}}},L0=function(t){return function(e){return function(r){return _e({reflectType:function(){return`<div>
  <pre><code>@txt@</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(o)(W()(W()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(o))(d.value)(U1)({txt:L(ar(ae)(`dgh d g h i =
  delay_ d [gain_ g [highpass_ h i]]

fade0 = bang
  $ P.gain
  $ AudioEnvelope { p: [1.0, 1.0, 0.0], o: 0.0, d: 8.0 }

fade1 = bang
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
  ]`)),ai0:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(a){return vt(n)([Sa(Gn(Ba)(a)(lt()))(function(u){return function(f){return ja(function(i){return Pt(ot)(1)([u,Zc(Qr)(ot)(Ja)(.15)(.7)(1500)([ja(function(m){return Lr(ot)(1)(q1(S(o))())([Zc(Qr)(ot)(Ja)(.4)(.5)(2500)([i,m])])})]),Zc(Qr)(ot)(Ja)(.29)(.85)(2e3)([ja(function(m){return Pt(ot)(1)([Zc(Qr)(ot)(Ja)(.6)(.6)(3500)([i,ja(function(s){return Lr(ot)(1)(H1(S(o))())([Zc(Qr)(ot)(Ja)(.75)(.6)(4e3)([m,s]),Zc(Qr)(ot)(Ja)(.75)(.55)(3e3)([u])])})])])})])])})}})])}})))})}}};var V1=function(){return d.value}(),W0=function(t){return function(e){return function(r){return function(n){var a=function(u){return Ta(O(c))(S(o))(n)(j(rt)(e(u))(fn))};return _e({reflectType:function(){return`<section>
  <p>
    In the <a @hwLink@ style="cursor:pointer;">hello world</a> section, we saw how to create and wire up two audio nodes: a <code>sinOsc</code>, or a sine-wave oscillator, is hooked up to a <code>gain</code> node. For some cases, feeding one audio node to another all the way up to a loudspeaker will be all you need. However, in most cases, you'll need to exploit three additional relationships:</p>
    <ul>
      <li><span style="font-weight:800px;">Many to one</span>, where many audio units pass through one.</li>
      <li><span style="font-weight:800px;">One to many</span>, where a single audio unit passes through many different ones.</li>
      <li><span style="font-weight:800px;">Feedback</span>, where an audio unit is an input to itself.</li>
    </ul>
    <p>This section will show how wags handles all three cases:</p>
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
  </section>`}})({reflectType:function(){return"@"}})()()(o)(an()(Q(c))({reflectType:function(){return"hwLink"}})({reflectSymbol:function(){return"hwLink"}})(o))(d.value)(V1)({hwLink:a(Lf.value)})}}}};var j1=function(){return d.value}(),B0=function(t){return function(e){return function(r){return function(n){var a=function(f){return Ta(O(c))(S(o))(n)(j(rt)(e(f))(fn))},u=xa(t)(r);return _e({reflectType:function(){return`<div>
  <h1>Array, fan, and fix</h1>

  <h3>The anatomy of a Wags graph</h3>

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

  <blockquote>If you don't have some sort of delay line in your processing chain, either via the Web-Audio-provided delay line or a custom delay node, Web Audio will raise a runtime error. Wags doesn't check for this, so make sure you test your audio to guarantee that it's feedback-explosion-free!</blockquote>

  <p>Nothing stops you from nesting <code>fix</code>-s to create a mega-feedback loop!</p>

  <blockquote>In the example below, I've added a couple fades to make sure the experience isn't too unpleasant. We'll talk more about fades in the events section \u{1F3B8}</blockquote>

  @code5@

  <h2>Next steps</h2>
  <p>In this section, saw how to combine together audio nodes with arrays, fan one audio node to many processing chains via <code>fan</code>, and how to create a fixed point, aka feedback, for a node via <code>fix</code>. In the next section, we'll ramp up on all of the yummy <a @next@ style="cursor:pointer;">audio nodes you can use</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(o)(W()(W()(W()(W()(W()(W()(W()(an()(Q(c))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))({reflectType:function(){return"intro"}})({reflectSymbol:function(){return"intro"}})(o))({reflectType:function(){return"code5"}})({reflectSymbol:function(){return"code5"}})(o))({reflectType:function(){return"code4"}})({reflectSymbol:function(){return"code4"}})(o))({reflectType:function(){return"code3"}})({reflectSymbol:function(){return"code3"}})(o))({reflectType:function(){return"code2"}})({reflectSymbol:function(){return"code2"}})(o))({reflectType:function(){return"code1"}})({reflectSymbol:function(){return"code1"}})(o))({reflectType:function(){return"code0"}})({reflectSymbol:function(){return"code0"}})(o))(d.value)(j1)({intro:L(W0(t)(e)(r)(n)),next:a(F_.value),code0:L(P0(u)(e)(n)),code1:L(O0(u)(e)(n)),code2:L(I0(u)(e)(n)),code3:L(R0(u)(e)(n)),code4:L(N0(u)(e)(n)),code5:L(L0(u)(e)(n))})}}}};var G0=function(t){return function(e){return function(r){return new w(V(t)("code")(e)(B(r)))}}},hb=function(t){return G0(t)(M(C(t.Monad0().Applicative0())))};var U0=function(t){return function(e){return function(r){return new w(V(t)("pre")(e)(B(r)))}}},Tb=function(t){return U0(t)(M(C(t.Monad0().Applicative0())))};var Y1=function(){return d.value}(),q0=function(t){return function(e){return function(r){return function(n){var a=j(rt)(e($_.value))(fn),u=xa(t)(r);return _e({reflectType:function(){return`<div>
  <h1>Hello world</h1>

  <h3>Wagging at 440Hz</h3>

  <p>Here's a "hello world" in Wags. In this and all the following sections, we'll start with a full example, and we'll pick it apart afterwards.</p>

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

  <p>Our sine wave oscillator is set to a frequency of <code>440Hz</code>. That means that your loudspeaker or headphones will vibrate back and forth in sinusoidal motion 440 times per second, which most folks perceive as the <a href="https://en.wikipedia.org/wiki/A440_(pitch_standard)">note A</a>. And we turn on the oscillator with <code>bangOn</code>, as the default is off for <i>all</i> sound generators in Wags. This is a design decision to help preserve the hearing of those that work frequently with audio.</p>

  <h2>Next steps</h2>
  <p>Now that we have our setup running, let's explore the anatomy of a Wags graph. Irrespective of the nodes comprising the graph, there are three basic concepts you need to be familiar with before you start diving into audio units: <a @next@ style="cursor:pointer;">array, fan, and fix</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(o)(W()(an()(W()(Q(c))({reflectType:function(){return"result"}})({reflectSymbol:function(){return"result"}})(o))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}})(o))(d.value)(Y1)({code:L(Tb(o)([hb(o)([ar(ae)(`case e of
  Just x -> x *> push Nothing
  _ -> (run2_ [ gain_ 0.15 [ sinOsc 440.0 bangOn ] ]
         >>= Just >>> push`)])])),result:L(et(St(n)(u)(function(f){return h(sa)(void 0)})(function(f){return function(i){return vt(f)([Pt(ot)(.15)([Nf(Yi)(440)(lt())])])}}))),next:Ta(O(c))(S(o))(n)(a)})}}}};var H0=ic;var z0=function(){return function(t){return t}};var V0=function(){return function(t){return t}};var xb=function(){function t(){}return t.value=new t,t}();var J0={attr:function(t){return function(e){return b({key:"height",value:G(e)})}}};var Fb=function(){function t(){}return t.value=new t,t}();var j0={attr:function(t){return function(e){return b({key:"width",value:G(e)})}}};var $b=function(t){return function(e){return function(r){return new w(V(t)("canvas")(e)(B(r)))}}};var Mb=function(){function t(){}return t.value=new t,t}(),wb={attr:function(t){return function(e){return b({key:"@self@",value:ft(e)})}}};function Qs(t){return function(){return t.getContext("2d")}}function H_(t){return function(e){return function(){t.fillStyle=e}}}function Ks(t){return function(){t.beginPath()}}function Ys(t){return function(){t.fill()}}function Pb(t){return function(e){return function(){t.arc(e.x,e.y,e.radius,e.start,e.end,e.useCounterClockwise)}}}function Zs(t){return function(e){return function(){t.fillRect(e.x,e.y,e.width,e.height)}}}var DL=function(){return 2*Xi}(),tl=function(t){return{o:t.value0+.04,n:t.value1,t:Wo}};var dL=function(){return d.value}(),el=function(t){return function(e){return function(r){return function(n){return K(t)(vo(e)(Tn)({p:[r,n],o:0,d:16}))}}}},bL=function(t){return function(e){return K(t)(gn(e)(Tn)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:18}))}},AL=function(t){return function(e){return K(t)(gn(e)(Tn)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:24}))}};var tm=function(t){return function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return function(i){return function(m){return function(s){return S_(t)(n)(a)([Lr(e)(u)(f)([BD(r)(i)(m)(s)])])}}}}}}}}}},X0=function(t){return function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return function(i){return function(m){return function(s){return S_(t)(n)(a)([Lr(e)(u)(f)([WD(r)(i)(m)(s)])])}}}}}}}}}},yL=function(t){return function(e){return function(r){return function(n){return K(t)(Vc(e)(Tn)({p:[r,n],o:0,d:16}))}}}},Q0=400,Ob=qe(Q0),kL=function(){return zt(za)(Q0)+"px"}(),K0=600,Ib=qe(K0),gL=function(){return zt(za)(K0)+"px"}(),CL={pluck0:"https://freesound.org/data/previews/493/493016_10350281-lq.mp3",pluck1:"https://freesound.org/data/previews/141/141524_2558140-lq.mp3",strum0:"https://freesound.org/data/previews/234/234738_3635427-lq.mp3"},Y0=function(t){return function(e){return function(r){return _e({reflectType:function(){return"<section>@ex1@</section>"}})({reflectType:function(){return"@"}})()()(o)(W()(Q(c))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(o))(d.value)(dL)({ex1:L(et(su()(o)(nr({reflectSymbol:function(){return"canvas"}})()()(nr({reflectSymbol:function(){return"slider"}})()()(wu({reflectSymbol:function(){return"startStop"}})()()()(nr({reflectSymbol:function(){return"loading"}})()()(nr({reflectSymbol:function(){return"start"}})()()(nr({reflectSymbol:function(){return"stop"}})()()(In)()()()())()()()())()()()())(In)()()()())()()()())()()()())(d.value)(function(n){return function(a){var u=I(O(c))(K(S(o))(void 0))(a.startStop.start),f=function(i){return function(m){return function(s){var _=p(k)(function(v){return new ut(v.acTime,v.value)})(jc(i)(a.slider));return[Cs(gs(kt(yt()(J(J(At)(ks)()()()({reflectSymbol:function(){return"fftSize"}}))(ys)()()()({reflectSymbol:function(){return"cb"}})))(bt()())))({cb:function(v){return function(){return En(new N(v))(s)(),En(z.value)(s)}},fftSize:cs.value})(h(fr)(Sa(Gn(Ba)(m)(I(O(c))(lt())(p(k)(function(){var v=Zn()(Ir),D=Bn(Dn)(Fa(0)(.96)(100)(1.04));return function(l){return v(tl(D(l)))}}())(_))))(function(v){return function(D){return ja(function(l){return Pt(ot)(1)([v,S_(PD(kt(yt()(J(J(At)(OC)()()()({reflectSymbol:function(){return"maxDelayTime"}}))(kD)()()()({reflectSymbol:function(){return"delayTime"}})))(bt()())))({maxDelayTime:2.5,delayTime:1})(p(k)(function(){var g=Vc()(Ir),nt=Bn(Dn)(Fa(0)(.5)(100)(2.45));return function(st){return g(tl(nt(st)))}}())(_))([Lr(ot)(.4)(p(k)(function(){var g=gn()(Ir),nt=Bn(Dn)(Fa(0)(.6)(100)(.9));return function(st){return g(tl(nt(st)))}}())(_))([v])]),tm(Qr)(ot)(Ja)(.15)(M(C(c)))(.7)(M(C(c)))(1500)(el(S(o))()(1500)(3e3))([ja(function(g){return Lr(ot)(1)(bL(S(o))())([tm(Qr)(ot)(Ja)(.4)(M(C(c)))(.5)(M(C(c)))(3e3)(el(S(o))()(3e3)(100))([l,g])])})]),tm(Qr)(ot)(Ja)(.29)(p(k)(function(){var g=Vc()(Ir),nt=Bn(Dn)(Fa(0)(.1)(100)(.4));return function(st){return g(tl(nt(st)))}}())(_))(.85)(M(C(c)))(2e3)(el(S(o))()(2e3)(5e3))([ja(function(g){return Pt(ot)(1)([tm(Qr)(ot)(Ja)(.6)(p(k)(function(){var nt=Vc()(Ir),st=Bn(Dn)(Fa(0)(.8)(100)(.3));return function(me){return nt(tl(st(me)))}}())(_))(.6)(M(C(c)))(3500)(el(S(o))()(3500)(100))([l,ja(function(nt){return Lr(ot)(1)(AL(S(o))())([X0(Qr)(ot)(OD)(.75)(p(k)(function(){var st=Vc()(Ir),me=Bn(Dn)(Fa(0)(.9)(100)(.1));return function(Qt){return st(tl(me(Qt)))}}())(_))(.6)(M(C(c)))(4e3)(el(S(o))()(4e3)(200))([g,nt]),X0(Qr)(ot)(OD)(.75)(yL(S(o))()(.75)(.2))(.55)(M(C(c)))(200)(el(S(o))()(200)(4e3))([v])])})])])})])])})}})))]}}};return Ge(o)([$b(o)(I(O(c))(_n(Xt)(C(c))(K(S(o)))([Y(j0)(Fb.value)(gL),Y(J0)(xb.value)(kL),Y(lk)(qt.value)("width: 100%;"),Y(wb)(Mb.value)(function(){var i=pr(c)(Ve)(function(m){return function(){var _=Qs(m)();return H_(_)("black")(),Zs(_)({width:Ib,height:Ob,x:0,y:0})(),void 0}});return function(m){return i(ab(m))}}())]))(p(k)(function(i){return Y(wb)(Mb.value)(function(){var m=pr(c)(Ve)(function(s){return function(){var v=Qs(s)();return H_(v)("black")(),Zs(v)({width:Ib,height:Ob,x:0,y:0})(),H_(v)("rgba(255,255,255,0.2)")(),sl(i)(function(D){return function(){return Ks(v)(),Pb(v)({end:DL,radius:D.value1*40,start:0,x:D.value0.x*Ib,y:D.value0.y*Ob,useCounterClockwise:!1})(),Ys(v)()}})()}});return function(s){return m(ab(s))}}())})(a.canvas)))([]),bo(o)(_n(Xt)(C(c))(K(S(o)))([Y(zo)(Do.value)("range"),Y(af)(Si.value)("0"),Y(nf)(Ei.value)("100"),Y(of)(Ti.value)("1"),Y(ff)(xi.value)("50"),Y(ck)(qt.value)("width: 100%;"),Y(uf)(hi.value)(Ke(function(){var i=pr(c)(Ve)(Jf(Cn)(Gf)(n.slider)),m=qn(ya)(zc);return function(s){return i(m(Yc(s)))}}()))]))([]),yn(o)(kr(Xt)(C(c))([K(S(o))(Y(fc)(qt.value)("width:100%; padding:1.0rem;")),_n(Xt)(C(c))(p(k)(function(){var i=Y(sr)(lr.value);return function(m){return i(Ke(T(m)))}}()))([X(k)(a.startStop.loading)(h(c)(void 0)),Ye(k)(a.startStop.stop)(function(i){return j(rt)(i)(j(rt)(t(h(c)(void 0)))(n.startStop.start(void 0)))}),Ye(k)(Sn(S(o))(I(O(c))(K(S(o))(h(c)(void 0)))(p(k)(function(i){return i.value0})(r)))(X(k)(u)(Z(tt))))(function(i){return function(){i(),n.startStop.loading(void 0)();var s=cr(z.value)(),_=mo(U(Or)(Yn(Pr))(function(v){return U(Or)(Wu(Pr)(v))(function(D){return U(Or)(p(bi)(V0())($E(Fn)(H0)(Ct(v))(z0()(CL))))(function(l){return U(Or)(mr(Pr)(Xl(0)(5e4)))(function(g){var nt=dc(Gv(Aa(Yl(l.pluck0))(mc(_v(pv()(l))))))({newSeed:sc(g),size:4});return mr(Pr)(function(){var me=jn(Zo)(c)(function(_t){return function(){var Te=Ou(),re=Ou();return{x:Te,y:re}}})(ln(0)(127))(),Qt=vt(v)(f(v)(nt)(s))(),Je=xt(tf)(function(_t){return function(){var Te=Mr(s)();return na(c)(Ve)(Te)(function(re){return function(){var Ao=O_(re)(),Bu=p(x)(function(){var Fi=El(me),ru=p(xe)(function(cn){return function(Jo){return Jo/255}(cn)});return function(cn){return Fi(ru(cn))}}())(Is(Os)(Ao))();return n.canvas(Bu)(),void 0}})()}})(),vr=j(rt)(j(rt)(j(rt)(Qt)(D))(kn(ur)(v)))(Je);return n.startStop.stop(vr)(),vr})})})})}))();return t(function(){return n.startStop.start(void 0)(),Uo(yi(_))()})(),void 0}})])]))([nn(ae)(kr(Xt)(C(c))([p(k)(T("Turn off"))(a.startStop.stop),p(k)(T("Turn on"))(u),p(k)(T("Loading..."))(a.startStop.loading)]))])])}})))})}}};var SL=function(){return d.value}(),Z0=function(t){return function(e){return function(r){return function(n){var a=xa(t)(r);return Ft({reflectType:function(){return`<div>
  <h1>Wags</h1>

  <h3>A web-audio framework written in PureScript</h3>

  <p>Hi! You've found <a href="https://github.com/mikesol/purescript-wags">Wags</a>.</p>

  <p>Wags is a web-audio framework designed for interactive media and games. Events like mouse clicks, MIDI notes and tweening frames are streamed to an audio rendering engine and, in response to these events, sound happens.</p>

  <h2>Why?</h2>

  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API">Web Audio API</a> is an amazing piece of technology. It is clear, concise, straightforward and ergonomic. So why build a framework on top of it?</p>

  <p>As audio projects become more and more ambitious, a need emerges for powerful abstractions to handle browser events and scheduling. Wags tackles this problem through a small set of <a href="https://en.wikipedia.org/wiki/Functional_reactive_programming">FRP</a>-based abstractions. In doing so, it aims to be concise, expressive, and as fast as manually-optimized hand-written JavaScript.</p>

  <h2>How does it sound?</h2>

  <p>Here's a small example in Wags that, when you turn it on, emits a single sound and then uses feedback loops to create long tail. You can use the slider to change the properties of the tail in real time.</p>

  ~ex~

  <p>By the end of this documentation, you'll know all of the concepts you need to create interactive audio like the example above.</p>

  <p>If you'd like to use this documentation as a springboard for your own work, it can be found <a href="https://github.com/mikesol/purescript-wags/tree/main/examples/docs">here</a>.</p>

  <p>And now, without further ado, let's write a small <a ~next~ style="cursor:pointer;">hello world \xE0 la wags</a>!</p>
</div>`}})()()(W()(an()(Q(c))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))({reflectType:function(){return"ex"}})({reflectSymbol:function(){return"ex"}})(o))(o)(SL)({next:Ta(O(c))(S(o))(n)(j(rt)(e(Lf.value))(fn)),ex:L(Y0(a)(e)(n))})}}}};var TL=function(){return d.value}(),th=function(t){return function(e){return function(r){return function(n){return Ft({reflectType:function(){return`<div>
  <h1>Merge and split</h1>

  <h3>Inputs and outputs abound!</h3>
  <p>
    Web audio allows you to merge and split arbitrary audio. This is essential when you're working with complex audio setups like 5.1 surround sound or novel headphones used in some gaming setups. Wags allows you to both split and merge arbitrary signals using Web Audio's native merger and splitter nodes.
  </p>

  <h2>Merging</h2>

  <p>Merging audio in wags looks like any node that takes multiple inputs, but instead of accepting something of type <code>AudioInput</code>, it accepts a <i>vector of audio inputs</i>.</p>

  <h2>Splitting</h2>

  <p>Splitting is the inverse operation of merging: it takes a single audio node and splits it into its separate channels. In doing so, it resembles <code>fan</code>, but instead of fanning the audio, it splits it into mono-channel audio.</p>

  <h2>Next steps</h2>
  <p>In this section, saw how to merge and split audio. In the next section, we'll look at how to work with <a ~next~ style="cursor:pointer;">custom audio worklets</a>.</p>
</div>`}})()()(an()(Q(c))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))(o)(TL)({next:K(S(o))(Y(sr)(lr.value)(Ke(T(j(rt)(e(hs.value))(fn)))))})}}}};var FL=function(){return d.value}(),eh=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
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
`}})()()(W()(W()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}})(o))(o)(FL)({txt:L(ar(ae)(`\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf OneOf.do
          bangOn
          delay 1000
            $ bang
            $ playbackRate
            $ AudioEnvelope
                { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                , o: 1.5
                , d: 30.0
                }
          delay 3000 (bang (playbackRate (AudioCancel { o: 3.5 })))
      ]
  ]`)),cancel:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([Pt(ot)(1)([ie(ge)(a)(kr(Xt)(C(c))([lt(),Mu(1e3)(wt(c)(Zn()(Tn)({p:Ha(Ko)(X(xe)(ln(0)(60))([1,1.2,1,.8])),o:1.5,d:30}))),Mu(3e3)(wt(c)(Zn()(yC)({o:3.5})))]))])])}})))})}}};var ML=function(){return d.value}(),rh=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
  <h2>Envelope</h2>
  <p>The <code>AudioEnvelope</code> parameter corresponds to the Web Audio API's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setValueCurveAtTime"><code>setValueCurveAtTime</code></a> function and sets an envelope <code>p</code> over the duration <code>d</code> starting at time <code>o</code>.</p>
  <pre><code>~txt~</code></pre>
  ~envelope~
  </section>
`}})()()(W()(W()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}})(o))(o)(ML)({txt:L(ar(ae)(`\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf
          OneOf.do
            bangOn
            delay 1000
              $ bang
              $ playbackRate
              $ AudioEnvelope
                  { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                  , o: 1.5
                  , d: 30.0
                  }
          )
      ]
  ]`)),envelope:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([Pt(ot)(1)([ie(ge)(a)(kr(Xt)(C(c))([lt(),Mu(1e3)(wt(c)(Zn()(Tn)({p:Ha(Ko)(X(xe)(ln(0)(60))([1,1.2,1,.8])),o:1.5,d:30})))]))])])}})))})}}};var PL=function(){return d.value}(),nh=function(t){return function(e){return function(r){return _e({reflectType:function(){return`<section>
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
            bang
              $ playbackRate
              $ AudioNumeric { n: 1.0, o: 1.0, t: _step }
            bang
              $ playbackRate
              $ AudioNumeric { n: 1.3, o: 2.0, t: _linear }
          delay 2500 OneOf.do
            bang
              $ playbackRate
              $ AudioNumeric { n: 1.0, o: 2.5, t: _step }
            bang
              $ playbackRate
              $ AudioNumeric { n: 0.7, o: 3.5, t: _exponential }
      ]
  ]</code></pre>

  @numericEx@
  </section>
`}})({reflectType:function(){return"@"}})()()(o)(W()(Q(c))({reflectType:function(){return"numericEx"}})({reflectSymbol:function(){return"numericEx"}})(o))(d.value)(PL)({numericEx:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([Pt(ot)(1)([ie(ge)(a)(Vo(O(c))(lt())(function(){return Vo(O(c))(Mu(1e3)(Vo(O(c))(wt(c)(Zn()(Ir)({n:1,o:1,t:DD})))(function(){return wt(c)(Zn()(Ir)({n:1.3,o:2,t:Wo}))})))(function(){return Mu(2500)(Vo(O(c))(wt(c)(Zn()(Ir)({n:1,o:2.5,t:DD})))(function(){return wt(c)(Zn()(Ir)({n:.7,o:3.5,t:AC}))}))})}))])])}})))})}}};var IL=function(){return d.value}(),ah=function(t){return function(e){return function(r){return _e({reflectType:function(){return`<section>
  <h2>AudioSudden</h2>
  <p>The simplest change you can make is scheduling a value to change <i>now</i>. This is done with <code>AudioSudden</code>, which is a wrapper around the setter for an audio parameter's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/value"><code>value</code></a> field in the Web Audio API.</p>

  <p>In the example below, we change a value after it has run for 1.5 seconds.</p>

  <pre><code>\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf OneOf.do
          bangOn
          delay 1500
            $ bang
            $ playbackRate
            $ AudioSudden { n: 1.4 }
      ]
  ]</code></pre>

  @suddenEx@
  </section>
`}})({reflectType:function(){return"@"}})()()(o)(W()(Q(c))({reflectType:function(){return"suddenEx"}})({reflectSymbol:function(){return"suddenEx"}})(o))(d.value)(IL)({suddenEx:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([Pt(ot)(1)([ie(ge)(a)(kr(Xt)(C(c))([lt(),Mu(1500)(wt(c)(Zn()(dC)({n:1.4})))]))])])}})))})}}};var NL=function(){return d.value}(),uh=function(t){return function(e){return function(r){return Ft({reflectType:function(){return`<section>
  <h2>Audio Units</h2>
  <p>In my humble opinion, the summit of Web Audio programming is when audio units control the audio parameters of other audio units. This allows for a form of radical experimentation that is difficult in many other frameworks. <a href="https://www.w3.org/TR/webaudio/#ModularRouting">Nearly any audio parameter</a> can be automated this way.</p>

  <p>To control an audio parameter with an audio unit, use the <code>AudioUnit</code> constructor. You can also use a <code>Node D1 l p</code>. If your node is for an arbitrary number of channels, make sure to coerce it to mono using the <code>c1</code> function, as in the example below.</p>

  <pre><code>\\ctx buf -> run2 ctx
  [ loopBuf buf OneOf.do
      bangOn
      bang
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
`}})()()(W()(Q(c))({reflectType:function(){return"unitEx"}})({reflectSymbol:function(){return"unitEx"}})(o))(o)(NL)({unitEx:L(et(St(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([ie(ge)(a)(kr(Xt)(C(c))([lt(),wt(c)(Zn()(mC(pi)(pi))(sC(Pt(ot)(1)([Ss(vs)(1)(lt()),Pt(ot)(.2)([Pc(ds)(100)([C_(Cc)(50)(lt())])])]))))]))])}})))})}}};var WL=function(){return d.value}(),oh=function(t){return function(e){return function(r){return function(n){var a=j(rt)(e(w_.value))(fn),u=xa(t)(r);return Ft({reflectType:function(){return`<div>
  <h1>Parameters</h1>

  <h3>Controlling our units</h3>
  <p>
    In the previous section, we saw how we can use browser events to control audio units. The Web Audio API provides a rich set of tools to control both the audio-rate and control-rate parameters of audio units. This section goes over how wags exposes those parameters.
  </p>

  ~sudden~
  ~numeric~
  ~envelope~
  ~cancel~
  ~unit~

  <h2>Next steps</h2>
  <p>In this section, we saw how to specify parameters for audio units, including using audio-rate audio units as parameters. In the next section, we'll look at how to make events <a ~next~ style="cursor:pointer;">stateful</a>.</p>
</div>`}})()()(W()(W()(an()(W()(W()(W()(Q(c))({reflectType:function(){return"unit"}})({reflectSymbol:function(){return"unit"}})(o))({reflectType:function(){return"sudden"}})({reflectSymbol:function(){return"sudden"}})(o))({reflectType:function(){return"numeric"}})({reflectSymbol:function(){return"numeric"}})(o))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}})(o))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}})(o))(o)(WL)({sudden:L(ah(u)(e)(n)),numeric:L(nh(u)(e)(n)),envelope:L(rh(u)(e)(n)),cancel:L(eh(u)(e)(n)),unit:L(uh(u)(e)(n)),next:Ta(O(c))(S(o))(n)(a)})}}}};var GL=function(){return d.value}(),ih=function(t){return function(e){return function(r){return function(n){return Ft({reflectType:function(){return`<div>
  <h1>Tumult</h1>

  <h2>The unknown unknowns of web audio</h2>
  <p>
    As we saw in the last section on subgraphs, sometimes, you simply don't know how your audio will evolve. For example, if we are building a live coding environment and create a space where someone can basically do <i>anything</i>, working with anonymous audio nodes will not cut it. For example, imagine that they have two relatively unconnected parts of the audio graph and want to start creating cross-connections. With subgraphs, we can't pick apart the graph and say "take node X and connect it to node Y" as nodes don't have names.
  </p>

  <p>
    Tumult solves this problem. It is an entirely separate rendering engine from the one we've seen before that works via <i>diffing</i> two audio graphs and adjusting the current graph based on the diff. The biggest difference is that tumult <i>requires</i> you to give a name to each audio unit. Otherwise, the engine won't know what node corresponds to what name.
  </p>

  <p>
    For you veteran coders and audio-ers, you may bristle at the word <i>diff</i>. After all, diffing is expensive, and we are working in resource constrained environments where timing is of the essence. Fear not! Tumult uses PureScript <code>Set</code>-s to do blazingly fast diffs. That said, tumult is the slowest part of wags (meaning it is just blazingly fast instead of obscenely and indecently fast), so only use it where you can afford a performmance hit.
  </p>

  <h2>Hello tumult</h2>

  <h2>Stability in tumult</h2>

  <p>While tumult will run a diff every time its event is triggered, if you don't tear down a node, its nodes will merrily putter along with whatever events you send them. This means that you can trigger tumult events <i>only</i> when you need to change the shape of the graph.</p>

  <h2>Feeling lucky</h2>

  <p>So far, we have only seen the <code>DiffMe</code> instruction sent to tumult. There is another instruction you can send called <code>FeelingLucky</code>. <code>FeelingLucky</code> will attempt to perform its instruction and be a no-op if it is incoherent with respect to the graph. Careful with this, though! <code>DiffMe</code> guarantees that whatever you send to tumult will be what's rendered from your loudspeaker. Too many <code>FeelingLucky</code>-s, on the other hand, can lead to bugs where you're not quite sure anymore <i>what</i> is playing. That said, <code>FeelingLucky</code> is grotesquely fast, especially for larger graphs. So if it makes sense to use it, use it!</p>

  <h2>Next steps</h2>
  <p>In this section, we learned how to use tumult to create truly dynamic audio graphs that allow you to fine-tune the speed-versus-dynamism tradeoff in various ways.</p>
  <p>In the next section, we'll look at how to create audio graphs via an <a ~next~ style="cursor:pointer;">imperative API that more closely resembles Web Audio while providing additional type-safety benefits</a>.</p>
</div>`}})()()(an()(Q(c))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))(o)(GL)({next:K(S(o))(Y(sr)(lr.value)(Ke(T(j(rt)(e(Ts.value))(fn)))))})}}}};var qL=function(){return d.value}(),fh=function(t){return function(e){return function(r){return function(n){return Ft({reflectType:function(){return`<div>
  <h1>Audio Worklets</h1>

  <h2>Supercharging our audio experiences</h2>

  <p>The Web Audio lets you do basically anything, but when anything isn't enough, there are custom Audio Worklets. </p>

  <p>Custom audio worklets come in a few shapes an sizes:</p>

  <ul>
    <li>You can <a href="https://developer.chrome.com/blog/audio-worklet/">write your own in JavaScript</a>. While this is ok for small PoCs, it will get really slow really quick.</li>
    <li>You can compile your favorite C/C++/Rust audio processors to <a href="https://developer.chrome.com/blog/audio-worklet-design-pattern/#using-audio-worklet-with-webassembly">web assembly</a>. The helpful folks at Google have created many examples that do exactly this, and you can pilfer them for fun and profit.</li>
    <li>My go-to solution is Faust. <a href="https://faust.grame.fr/">Faust</a> is the greatest audio project on Earth, and let's you build very performant audio processors with an expressive and concise syntax. Faust's <a href="https://webaudioconf.com/posts/2017_EA_60/">Web Audio integration</a> is a one-liner from the command line.</li>
  </ul>

  <p>In this example, we'll use Faust to create a three custom audio units and wire them together using Wags. The units are:</p>

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

  We can compile each example using the following commands. Each command creates a self-contained JavaScript file, so all we need to do is link to it in wags-land.

  <pre><code>placeholder</code></pre>

  <h2>Wags</h2>

  <p>Wags provides a type-safe interface for declaring the API of Audio Worklets. While it's your job to make sure the API is in fact the actual API of the worklet, assuming this is correct, wags will enforce it for you. Let's see how in the following example. Here's the code:</p>

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
  <p>In this section, we created three audio worklet nodes using Faust and used them in the Web Audio API via wags. There is active work going on to bundle all this into a single toolchain so that Faust can be written directly in PureScript and automatically read as an Audio Worklet by wags. Until that happens, though, this is a great solution: just make sure to get the parameter names right across the language barrier! No amonut of type-safety can save you there \u{1F605}</p>
  <p>In the next section, we'll look at how to create <a ~next~ style="cursor:pointer;">mutable state in a wags graph</a>.</p>
</div>`}})()()(an()(Q(c))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))(o)(qL)({next:K(S(o))(Y(sr)(lr.value)(Ke(T(j(rt)(e(Wf.value))(fn)))))})}}}};var zL=function(){return d.value}(),ch=function(t){return function(e){return function(r){return function(n){return _e({reflectType:function(){return`<div>
  <h1>Imperative API</h1>

  <h2>Like JavaScript, but PureScript</h2>
  <p>
    If you're coming from the JavaScript or TypeScript world, or if you're a fan of monadic <code>do</code> notation, you may enjoy building things step-by-step rather than constructing large declarative structures. If you're that sort of person, this section is for you!
  </p>

  <h2>Parting shot</h2>
  <p>Thanks for checking out wags! We want it to be the most ergonomimc, expressive, and performant Web Audio API on your side of the Mississippi. It certainly is for me, and as I'm in Finland, I'm on <i>both sides</i> of the Mississippi, so you can't beat that! If you have any questions, comments, concerns or would just like to say "hi!", please check out the <a href="https://github.com/mikesol/purescript-wags">Wags GitHub Repo</a> or the <a href="https://purescript.org/chat">PureScript Discord's music channel</a>. Happy wagging!</p>
</div>`}})({reflectType:function(){return"~"}})()()(o)(Q(c))(d.value)(zL)({})}}}};var JL=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst)
import QualifiedDo.Alt as OneOf
import Data.Tuple.Nested ((/\\))
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (switcher, text, text_)
import Deku.Core (Domable, envy)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Random as Random
import FRP.Behavior (Behavior, behavior, sampleBy)
import FRP.Event (create, fold, makeEvent, subscribe, delay)
import FRP.Event.Class (bang)
import FRP.Event.VBus (V, vbus)
import QualifiedDo.OneOfMap as O
import Type.Proxy (Proxy(..))
import WAGS.Control (gain_, playBuf)
import WAGS.Core (Channel(..), dyn, bangOn)
import WAGS.Interpret (bracketCtx, decodeAudioDataFromUri)
import WAGS.Run (run2_)
import WAGS.WebAPI (BrowserAudioBuffer)

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
    :: forall lock payload
     . Maybe BrowserAudioBuffer
    -> Domable Effect lock payload
  scene = maybe (D.div_ [ text_ "Loading..." ]) \\buffer ->
    D.div_ $ pure $ envy $ vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        startE = bang unit <|> event.startStop.start
        sl = sampleBy (/\\) random
          $ fold (\\_ b -> b + 1) event.slider 0
        music = run2_
          [ gain_ 1.0
              [ dyn $ map
                  ( \\i ->
                      OneOf.do
                        bang $ sound $ playBuf
                          { buffer: buffer, playbackRate: 0.7 + (fst i) * 2.0 }
                          bangOn
                        delay 5000 $ bang $ silence
                  )
                  sl
              ]
          ]
      D.div_
        [ D.div_
            [ text_ "Slide me!"
            , D.input
                ( O.oneOfMap bang O.do
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
`,jL=tu(function(t){return Wt(function(e){return xt(t)(function(r){return function(){var a=Ou();return e(r(a))()}})})}),XL=function(){return d.value}(),QL="https://freesound.org/data/previews/339/339810_5121236-lq.mp3",lh=function(t){return function(e){return function(r){return _e({reflectType:function(){return`<section>
  <h2>Hello subgraph</h2>

  <p>Subgraphs have the type <code>Event (Event (Channel outputChannels lock payload))</code>. Streaming audio is a data type with two constructors: <code>sound</code> to create a subgraph and <code>silence</code> to turn it off. The inner event listens for sound/silence, and the outer event adds subgraphs to the scene. You can create as many subgraphs as you like: wags automatically frees up resources when you send the <code>silence</code> event. Note that, once you turn a subraph off with <code>silence</code>, you can't turn it back on again. In this case, just create a new subgraph.</p>

  <p>Here's a simple subgraph that is connected to a slider. As you slide the slider, new nodes are provisioned. Each one has a pseudo-random pitch.</p>

  <pre><code>@txt@</code></pre>
  @ex1@

</section>
`}})({reflectType:function(){return"@"}})()()(o)(W()(W()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(o))(d.value)(XL)({txt:L(ar(ae)(JL)),ex1:L(et(su()(o)(nr({reflectSymbol:function(){return"slider"}})()()(wu({reflectSymbol:function(){return"startStop"}})()()()(nr({reflectSymbol:function(){return"loading"}})()()(nr({reflectSymbol:function(){return"start"}})()()(nr({reflectSymbol:function(){return"stop"}})()()(In)()()()())()()()())()()()())(In)()()()())()()()())(d.value)(function(n){return function(a){var u=I(O(c))(K(S(o))(void 0))(a.startStop.start),f=Ci(S(o))(ut.create)(jL)($u(S(o))(function(m){return function(s){return s+1|0}})(a.slider)(0)),i=function(m){return[Pt(ot)(1)([uv(p(k)(function(s){return kr(Xt)(C(c))([K(S(o))(_C(Gn(y_(kt(yt()(J(J(At)(TC)()()()({reflectSymbol:function(){return"playbackRate"}}))(d_)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())))({buffer:m,playbackRate:.7+Na(s)*2})(lt()))),Mu(5e3)(K(S(o))(pC))])})(f))])]};return Ge(o)([Ge(o)([ar(ae)("Slide me!"),bo(o)(_n(Xt)(C(c))(K(S(o)))([Y(zo)(Do.value)("range"),Y(af)(Si.value)("0"),Y(nf)(Ei.value)("100"),Y(of)(Ti.value)("1"),Y(ff)(xi.value)("50"),Y(uf)(hi.value)(Ke(T(n.slider(void 0))))]))([])]),yn(o)(_n(Xt)(C(c))(p(k)(function(){var m=Y(sr)(lr.value);return function(s){return m(Ke(T(s)))}}()))([X(k)(a.startStop.loading)(h(c)(void 0)),Ye(k)(a.startStop.stop)(function(m){return j(rt)(m)(j(rt)(t(h(c)(void 0)))(n.startStop.start(void 0)))}),Ye(k)(Sn(S(o))(I(O(c))(K(S(o))(h(c)(void 0)))(p(k)(function(m){return m.value0})(r)))(X(k)(u)(Z(tt))))(function(m){return function(){m(),n.startStop.loading(void 0)();var _=mo(U(Or)(Yn(Pr))(function(v){return U(Or)(Wu(Pr)(v))(function(D){return U(Or)(Ct(v)(QL))(function(l){return mr(Pr)(function(){var nt=Ps(i(l))(),st=j(rt)(j(rt)(nt)(D))(kn(ur)(v));return n.startStop.stop(st)(),st})})})}))();return t(function(){return n.startStop.start(void 0)(),Uo(yi(_))()})(),void 0}})]))([nn(ae)(kr(Xt)(C(c))([p(k)(T("Turn off"))(a.startStop.stop),p(k)(T("Turn on"))(u)]))])])}})))})}}};var YL=function(){return d.value}(),_h=function(t){return function(e){return function(r){return function(n){var a=xa(t)(r);return Ft({reflectType:function(){return`<div>
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
  <p>Thus ends the first version of the wags documentation. Applause is always welcome ~appl~! Alas, some features remain undocumented, like audio worklets and an imperative API. At some point I hope to document all of these, but hopefully this should be enough to get anyone interested up and running. If you need to use any of those features before I document them, ping me on the <a href="https://purescript.org/chat">PureScript Discord</a>. Otherwise, happy music making with Wags!</p>
</div>`}})()()(W()(W()(Q(c))({reflectType:function(){return"suby"}})({reflectSymbol:function(){return"suby"}})(o))({reflectType:function(){return"appl"}})({reflectSymbol:function(){return"appl"}})(o))(o)(YL)({appl:L(et(ws("\u{1F44F}")(n)(a)(function(u){return Ct(u)("https://freesound.org/data/previews/277/277021_1402315-lq.mp3")})(function(u){return function(f){return vt(u)([Pt(ot)(1)([ie(ge)(f)(lt())])])}}))),suby:L(lh(a)(e)(n))})}}}};var sLt=function(t){return t},mLt={Coercible0:function(){}},tW=function(t){return function(e){var r=function(a){var u=function(f){if(f instanceof x_)return Ge(o)(h(fr)(et(jr(o)(Z0(a.setCancellation)(a.setPage)))));if(f instanceof Lf)return Ge(o)(h(fr)(et(jr(o)(q0(a.setCancellation)(a.setPage)))));if(f instanceof $_)return Ge(o)(h(fr)(et(jr(o)(B0(a.setCancellation)(a.setPage)))));if(f instanceof F_)return Ge(o)(h(fr)(et(jr(o)(LS(a.setCancellation)(a.setPage)))));if(f instanceof hs)return Ge(o)(h(fr)(et(jr(o)(fh(a.setCancellation)(a.setPage)))));if(f instanceof Wf)return Ge(o)(h(fr)(et(jr(o)(w0(a.setCancellation)(a.setPage)))));if(f instanceof M_)return Ge(o)(h(fr)(et(jr(o)(oh(a.setCancellation)(a.setPage)))));if(f instanceof w_)return Ge(o)(h(fr)(et(jr(o)(y0(a.setCancellation)(a.setPage)))));if(f instanceof Ts)return Ge(o)(h(fr)(et(jr(o)(ch(a.setCancellation)(a.setPage)))));if(f instanceof qE)return Ge(o)(h(fr)(et(jr(o)(th(a.setCancellation)(a.setPage)))));if(f instanceof P_)return Ge(o)(h(fr)(et(jr(o)(_h(a.setCancellation)(a.setPage)))));if(f instanceof HE)return Ge(o)(h(fr)(et(jr(o)(ih(a.setCancellation)(a.setPage)))));throw new Error("Failed pattern match at WAGS.Example.Docs (line 144, column 5 - line 144, column 80): "+[f.constructor.name])};return u(a.page)},n=$u(S(o))(function(a){if(a instanceof Wc)return function(u){return{prevPage:new N(u.curPage),curPage:a.value0,cancel:u.cancel,pageChange:!0}};if(a instanceof HD)return function(u){return{cancel:a.value0,pageChange:!1,curPage:u.curPage,prevPage:u.prevPage}};throw new Error("Failed pattern match at WAGS.Example.Docs (line 134, column 7 - line 136, column 75): "+[a.constructor.name])})(e)({prevPage:z.value,curPage:x_.value,cancel:h(c)(void 0),pageChange:!0});return[Ge(o)(p(xe)(function(a){return Av(o)([bv(o)(I(O(c))(_n(Xt)(C(c))(K(S(o)))([Y(sr)(lr.value)(Ke(T(t(new Wc(a.value0))))),Y(_k)(qt.value)("cursor:pointer;")]))(p(k)(function(u){return Y(sr)(lr.value)(Ke(T(function(){return u.cancel(),t(new Wc(a.value0))()})))})(Ml(pu(c))(function(){var u=fu(Ia);return function(f){return u(function(i){return i.pageChange}(f))}}())(n))))([ar(ae)(a.value1.value0)]),lc(o)(K(S(o))(Y(Pp)(qt.value)(function(){return a.value1.value1?"":"display:none;"}())))([ar(ae)(" | ")])])})([new ut(x_.value,new ut("Home",!0)),new ut(Lf.value,new ut("Hello world",!0)),new ut($_.value,new ut("Array, fan, and fix",!0)),new ut(F_.value,new ut("Audio units",!0)),new ut(Wf.value,new ut("Events",!0)),new ut(M_.value,new ut("Parameters",!0)),new ut(w_.value,new ut("State",!0)),new ut(P_.value,new ut("Subgraphs",!1))])),Ge(o)(h(fr)(vv(o)(function(a){return r({page:a.curPage,setPage:function(u){return t(Wc.create(u))},setCancellation:function(u){return t(HD.create(u))}})})(Ml(pu(c))(function(a){return a.pageChange})(n))))]}},vLt=function(t){return{page:t,setPage:Ot(Hr(zr(Zr))),setCancellation:Ot(Hr(zr(Zr)))}},DLt=function(){var e=U(Cn)(U(Cn)(gi)(ob))(aS)();return na(c)(Ve)(p($r)(uS)(e))(function(r){return function(){var a=kv(),u=cr(0)(),f=Nl(o)(o)(),i=fk(o)(r)(tW(f.push)(f.event))(ug(u));return Ae(x)(xt(i)(function(m){return m(a)}))(),f.push(new Wc(x_.value))()}})()};export{sLt as TopLevelSg,DLt as main,mLt as newtypeTopLevelSg_,vLt as p2tl,tW as scene};
