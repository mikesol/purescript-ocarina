function Hb(t){return()=>t.slice()}function zb(t){return e=>r=>()=>{r[t]=e}}function Vb(t){return()=>t.slice()}var Jb=function(t){return function(e){return function(r){return function(n){return function(a){return n<a?t:n===a?e:r}}}}};var jb=Jb,Xb=Jb;var Qb=function(t){return function(e){return t===e}};var Kb=Qb,Yb=Qb;var d=function(){function t(){}return t.value=new t,t}();var Ar=function(t){return t.reflectSymbol};var ll=function(t){return function(e){return{}.hasOwnProperty.call(e,t)}},Oa=function(t){return function(e){return e[t]}},Vu=function(t){return function(e){return function(r){var n={};for(var a in r)({}).hasOwnProperty.call(r,a)&&(n[a]=r[a]);return n[t]=e,n}}};var _l={eq:Yb},Oi={eq:Kb};var Zt=function(t){return t.eq};var te=function(){function t(){}return t.value=new t,t}(),be=function(){function t(){}return t.value=new t,t}(),ye=function(){function t(){}return t.value=new t,t}();var Zb=function(t){return function(e){return t-e|0}},ty=function(t){return function(e){return t-e}};var ey=function(t){return function(e){return t+e|0}},ry=function(t){return function(e){return t*e|0}},ny=function(t){return function(e){return t+e}},ay=function(t){return function(e){return t*e}};var ka=function(t){return t.zero};var ga={add:ny,zero:0,mul:ay,one:1},Fu={add:ey,zero:0,mul:ry,one:1};var Ca=function(t){return t.one};var In=function(t){return t.mul};var We=function(t){return t.add};var $u=function(t){return t.sub};var mf={sub:ty,Semiring0:function(){return ga}},_m={sub:Zb,Semiring0:function(){return Fu}};var pl=function(t){return function(e){return $u(t)(ka(t.Semiring0()))(e)}};var Ia=function(){return{compare:Xb(te.value)(ye.value)(be.value),Eq0:function(){return _l}}}(),Xr=function(){return{compare:jb(te.value)(ye.value)(be.value),Eq0:function(){return Oi}}}();var ee=function(t){return t.compare};var oy=function(t){return function(e){return function(r){var n=ee(t)(e)(r);return!(n instanceof te)}}};var Mu=function(t){return function(e){return function(r){var n=ee(t)(e)(r);if(n instanceof te)return r;if(n instanceof ye||n instanceof be)return e;throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): "+[n.constructor.name])}}};var sm=function(t){return function(e){return function(r){var n=oy(t)(r)(ka(e.Semiring0()));return n?r:pl(e)(r)}}};var iy=function(t){return function(e){for(var r=t.length,n=e.length,a=new Array(r*n),u=0,i=0;i<r;i++)for(var o=t[i],m=0;m<n;m++)a[u++]=o(e[m]);return a}};var Yo={compose:function(t){return function(e){return function(r){return t(e(r))}}}},wu=function(t){return t.compose};var tt=function(t){return t.identity},et={identity:function(t){return t},Semigroupoid0:function(){return Yo}};var nr=!0;var xt=function(t){return function(e){return function(r){return t(r)(e)}}},x=function(t){return function(e){return t}};var Qf=function(t){return function(e){return e(t)}},ml=function(t){return function(e){return t(e)}};var fy=function(t){return function(e){for(var r=e.length,n=new Array(r),a=0;a<r;a++)n[a]=t(e[a]);return n}};var _=function(t){return t.map},Qe=function(t){return function(e){return function(r){return _(t)(r)(e)}}},Ae=function(t){return _(t)(x(void 0))},Q=function(t){return function(e){return function(r){return _(t)(x(r))(e)}}},Z_=function(t){return function(e){return _(t)(x(e))}};var fa={map:wu(Yo)},$e={map:fy},mm=function(t){return function(e){return function(r){return _(t)(function(n){return n(r)})(e)}}};var vl={apply:iy,Functor0:function(){return $e}},Ut=function(t){return t.apply};var X=function(t){return function(e){return function(r){return Ut(t)(_(t.Functor0())(x(tt(et)))(e))(r)}}},ca=function(t){return function(e){return function(r){return function(n){return Ut(t)(_(t.Functor0())(e)(r))(n)}}}};var T=function(t){return t.pure};var Rn=function(t){return function(e){return function(r){if(e)return r;if(!e)return T(t)(void 0);throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): "+[e.constructor.name,r.constructor.name])}}},Dl=function(t){return function(e){return function(r){return Ut(t.Apply0())(T(t)(e))(r)}}};var pr={pure:function(t){return[t]},Apply0:function(){return vl}};var cy=function(t){return function(e){for(var r=[],n=0,a=t.length;n<a;n++)Array.prototype.push.apply(r,e(t[n]));return r}};var ar=function(t){return t.discard};var Zo={bind:cy,Apply0:function(){return vl}},q=function(t){return t.bind},Qn=function(t){return xt(q(t))};var Kf=function(t){return function(e){return function(r){return function(n){return q(t)(e(n))(r)}}}};var je={discard:function(t){return q(t)}};var Ja=function(t){return function(e){return q(t)(e)(tt(et))}};var lt=function(t){return t};var ly=lt;var _y=function(t){return function(e){return function(){return t(e())}}};function la(t){return function(){return{value:t}}}var Nn=function(t){return function(){return t.value}},py=function(t){return function(e){return function(){var r=t(e.value);return e.value=r.state,r.value}}},ja=function(t){return function(e){return function(){return e.value=t}}};var ju=function(t){return function(e){return function(r){return q(t.Bind1())(e)(function(n){return q(t.Bind1())(r)(function(a){return T(t.Applicative0())(n(a))})})}}};var sy=function(t){return function(e){return t.length===0?e:e.length===0?t:t.concat(e)}};var Df=function(t){var e=function(r){var n;function a(u){r=u}for(;;)n=a(r);return n};return e(t)};var my={append:function(t){return function(e){return void 0}}};var Sn={append:sy};var bt=function(t){return t.append},dm=function(t){return{append:function(e){return function(r){return function(n){return bt(t)(e(n))(r(n))}}}}};var N=function(t){return t.alt};var Bh=String.fromCharCode(65535),Gh=String.fromCharCode(0),Uh=Number.POSITIVE_INFINITY,qh=Number.NEGATIVE_INFINITY;var Kn=function(t){return t.top};var df={top:2147483647,bottom:-2147483648,Ord0:function(){return Xr}};var Yn=function(t){return t.bottom};var Dy=function(t){return t.toString()},dy=function(t){var e=t.toString();return isNaN(e+".0")?e:e+".0"};var ep={show:dy},Xa={show:Dy};var Ht=function(t){return t.show};var V=function(){function t(){}return t.value=new t,t}(),B=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var Yt=function(t){return function(e){return function(r){if(r instanceof V)return t;if(r instanceof B)return e(r.value0);throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}};var Or={map:function(t){return function(e){return e instanceof B?new B(t(e.value0)):V.value}}};var Ea=function(t){return Yt(t)(tt(et))},ta=function(){return function(t){if(t instanceof B)return t.value0;throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): "+[t.constructor.name])}};var Ri={apply:function(t){return function(e){if(t instanceof B)return _(Or)(t.value0)(e);if(t instanceof V)return V.value;throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): "+[t.constructor.name,e.constructor.name])}},Functor0:function(){return Or}},Sa={bind:function(t){return function(e){if(t instanceof B)return e(t.value0);if(t instanceof V)return V.value;throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): "+[t.constructor.name,e.constructor.name])}},Apply0:function(){return Ri}};var ho=function(){return{pure:B.create,Apply0:function(){return Ri}}}();var jt=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Xt=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var yf={map:function(t){return function(e){if(e instanceof jt)return new jt(e.value0);if(e instanceof Xt)return new Xt(t(e.value0));throw new Error("Failed pattern match at Data.Either (line 31, column 1 - line 31, column 52): "+[e.constructor.name])}}};var Ra=function(t){return function(e){return function(r){if(r instanceof jt)return t(r.value0);if(r instanceof Xt)return e(r.value0);throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}},rp=function(){return Ra(x(V.value))(B.create)}();var pu=function(t){return t};var To={map:function(t){return function(e){return t(e)}}};var by={apply:function(t){return function(e){return t(e)}},Functor0:function(){return To}},tT={bind:function(t){return function(e){return e(t)}},Apply0:function(){return by}},ym={pure:pu,Apply0:function(){return by}},Qu={Applicative0:function(){return ym},Bind1:function(){return tT}};var yy=function(t){return Math.min(Math.abs(t),2147483647)},Ay=function(t){return function(e){return e===0?0:e>0?Math.floor(t/e):-Math.floor(t/-e)}},ky=function(t){return function(e){if(e===0)return 0;var r=Math.abs(e);return(t%r+r)%r}},gy=function(t){return function(e){return t/e}};var Cy={Ring0:function(){return mf}},Ey={Ring0:function(){return _m}};var su=function(t){return t.mod};var yl={degree:function(t){return 1},div:gy,mod:function(t){return function(e){return 0}},CommutativeRing0:function(){return Cy}},xo={degree:yy,div:Ay,mod:ky,CommutativeRing0:function(){return Ey}},Ku=function(t){return t.div};var Ir={mempty:void 0,Semigroup0:function(){return my}};var Pt=function(t){return t.mempty},Qr=function(t){return{mempty:function(e){return Pt(t)},Semigroup0:function(){return dm(t.Semigroup0())}}};var Am=function(t){return function(){return t}},Sy=function(t){return function(e){return function(){return e(t())()}}};var Al=function(t){return function(e){return function(){for(var r=0,n=t.length;r<n;r++)e(t[r])()}}};var hy=function(t,e,r){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return n=1,a=r(),n=2,a}},ue={Applicative0:function(){return f},Bind1:function(){return hn}},hn={bind:Sy,Apply0:function(){return km(0)}},f={pure:Am,Apply0:function(){return km(0)}},Ty=hy("functorEffect","Effect",function(){return{map:Dl(f)}}),km=hy("applyEffect","Effect",function(){return{apply:ju(ue),Functor0:function(){return Ty(0)}}}),F=Ty(20),nt=km(23),xy=function(t){return{append:ca(nt)(bt(t))}},gr=function(t){return{mempty:Am(Pt(t)),Semigroup0:function(){return xy(t.Semigroup0())}}};var Fy=function(t){return function(){return{value:t}}};var Rr=function(t){return function(){return t.value}},$y=function(t){return function(e){return function(){var r=t(e.value);return e.value=r.state,r.value}}},Tn=function(t){return function(e){return function(){e.value=t}}};var sr=Fy,oT=$y,tc=function(t){return oT(function(e){var r=t(e);return{state:r,value:r}})},Af=function(t){return function(e){return Ae(F)(tc(t)(e))}};var _T=py,Pu=function(t){return _T(function(e){var r=t(e);return{state:r,value:r}})},Ni={map:_y};var k={liftST:ly,Monad0:function(){return ue}},Me=function(t){return t.liftST};var pn=function(t){return function(e){for(var r=t>e?-1:1,n=new Array(r*(e-t)+1),a=t,u=0;a!==e;)n[u++]=a,a+=r;return n[u]=a,n}},sT=function(t){return function(e){if(t<1)return[];var r=new Array(t);return r.fill(e)}},mT=function(t){return function(e){for(var r=[],n=0,a=0;a<t;a++)r[n++]=e;return r}},np=typeof Array.prototype.fill=="function"?sT:mT,vT=function(){function t(a,u){this.head=a,this.tail=u}var e={};function r(a){return function(u){return new t(a,u)}}function n(a){for(var u=[],i=0,o=a;o!==e;)u[i++]=o.head,o=o.tail;return u}return function(a){return function(u){return n(a(r)(e)(u))}}}(),Ou=function(t){return t.length};var wy=function(t){return function(e){return function(r){return function(n){for(var a=0,u=n.length;a<u;a++)if(r(n[a]))return t(a);return e}}}};var Py=function(t){return function(e){return function(r){return function(n){if(r<0||r>=n.length)return e;var a=n.slice();return a.splice(r,1),t(a)}}}};var DT=function(){function t(e,r,n,a,u,i){var o,m,s,l,v,D,c;for(o=u+(i-u>>1),o-u>1&&t(e,r,a,n,u,o),i-o>1&&t(e,r,a,n,o,i),m=u,s=o,l=u;m<o&&s<i;)v=a[m],D=a[s],c=r(e(v)(D)),c>0?(n[l++]=D,++s):(n[l++]=v,++m);for(;m<o;)n[l++]=a[m++];for(;s<i;)n[l++]=a[s++]}return function(e){return function(r){return function(n){var a;return n.length<2?n:(a=n.slice(0),t(e,r,a,n.slice(0),0,n.length),a)}}}}();var kl=function(t){return function(e){return function(r){for(var n=e.length<r.length?e.length:r.length,a=new Array(n),u=0;u<n;u++)a[u]=t(e[u])(r[u]);return a}}};var Oy=function(t){return function(e){return t[e]}};var bT=function(t,e,r){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return n=1,a=r(),n=2,a}};var Iy={defer:function(t){return function(e){return t(void 0)(e)}}},ec=function(t){return t.defer},Cm=function(t){return function(e){var r=bT("go","Control.Lazy",function(){return ec(t)(function(a){return e(r(25))})}),n=r(25);return n}};var yT=function(){function t(e,r,n,a,u,i){var o,m,s,l,v,D,c;for(o=u+(i-u>>1),o-u>1&&t(e,r,a,n,u,o),i-o>1&&t(e,r,a,n,o,i),m=u,s=o,l=u;m<o&&s<i;)v=a[m],D=a[s],c=r(e(v)(D)),c>0?(n[l++]=D,++s):(n[l++]=v,++m);for(;m<o;)n[l++]=a[m++];for(;s<i;)n[l++]=a[s++]}return function(e){return function(r){return function(n){return function(){return n.length<2||t(e,r,n,n.slice(0),0,n.length),n}}}}}();var By=function(t){return function(e){return t&&e}},Gy=function(t){return function(e){return t||e}},Uy=function(t){return!t};var Du=function(t){return t.not};var nc=function(t){return t.disj},La={ff:!1,tt:!0,implies:function(t){return function(e){return nc(La)(Du(La)(t))(e)}},conj:By,disj:Gy,not:Uy};var Hy=function(t){return function(e){return function(r){for(var n=e,a=r.length,u=a-1;u>=0;u--)n=t(r[u])(n);return n}}},zy=function(t){return function(e){return function(r){for(var n=e,a=r.length,u=0;u<a;u++)n=t(n)(r[u]);return n}}};var w=function(t){return t.empty};var at=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}(),ac=function(t){return function(e){return t(e.value0)(e.value1)}};var rn=function(t){return t.value1};var ti={map:function(t){return function(e){return new at(e.value0,t(e.value1))}}};var Ba=function(t){return t.value0};var RT=function(t){return{append:function(e){return function(r){return wu(t)(e)(r)}}}};var xl=function(t){return{mempty:tt(t),Semigroup0:function(){return RT(t.Semigroupoid0())}}};var Ln=function(){return lt};var nn=Ln,Cr=Ln;var $m=function(){return function(){return function(t){return Ln()}}};var er=function(t){return t.foldr};var Sr=function(t){return function(e){return er(t)(N(e.Alt0()))(w(e))}},sn=function(t){return function(e){return function(r){return er(t)(function(){var n=N(e.Alt0());return function(a){return n(r(a))}}())(w(e))}}},vr=function(t){return function(e){return function(r){return er(e)(function(){var n=X(t.Apply0());return function(a){return n(r(a))}}())(T(t)(void 0))}}},_a=function(t){return function(e){return xt(vr(t)(e))}},lp=function(t){return function(e){return vr(t)(e)(tt(et))}},Dr=function(t){return t.foldl};var Ke={foldr:function(t){return function(e){return function(r){if(r instanceof V)return e;if(r instanceof B)return t(r.value0)(e);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}},foldl:function(t){return function(e){return function(r){if(r instanceof V)return e;if(r instanceof B)return t(e)(r.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}},foldMap:function(t){return function(e){return function(r){if(r instanceof V)return Pt(t);if(r instanceof B)return e(r.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[e.constructor.name,r.constructor.name])}}}};var Qy=function(t){return function(e){return function(r){return er(t)(function(n){return function(a){return bt(e.Semigroup0())(r(n))(a)}})(Pt(e))}}},zt={foldr:Hy,foldl:zy,foldMap:function(t){return Qy(zt)(t)}};var xn=function(t){return t.foldMap};var Ky=function(){function t(a){return[a]}function e(a){return function(u){return[a,u]}}function r(a){return function(u){return function(i){return[a,u,i]}}}function n(a){return function(u){return a.concat(u)}}return function(a){return function(u){return function(i){return function(o){return function(m){function s(l,v){switch(v-l){case 0:return i([]);case 1:return u(t)(o(m[l]));case 2:return a(u(e)(o(m[l])))(o(m[l+1]));case 3:return a(a(u(r)(o(m[l])))(o(m[l+1])))(o(m[l+2]));default:var D=l+Math.floor((v-l)/4)*2;return a(u(n)(s(l,D)))(s(D,v))}}return s(0,m.length)}}}}}}();var Wn=function(t){return t.traverse};var oA=function(t){return function(e){return Wn(t)(e)(tt(et))}},ei={traverse:function(t){return Ky(Ut(t.Apply0()))(_(t.Apply0().Functor0()))(T(t))},sequence:function(t){return oA(ei)(t)},Functor0:function(){return $e},Foldable1:function(){return zt}};var wl=function(){return kl(at.create)}();var qm=function(){return Oy};var sA=function(t){return[t]};var mA=function(){return wy(B.create)(V.value)}();var Hm=function(){return Py(B.create)(V.value)}(),zm=function(t){return function(e){return function(r){return r.length===0?[]:Yt(r)(function(n){return ta()(Hm(n)(r))})(mA(t(e))(r))}}};var Bi=function(t){return function(e){return bt(Sn)([t])(e)}};var vA=function(t){return function(e){for(var r=e.length,n=Array(r),a=0;a<r;a++)n[a]=t(a)(e[a]);return n}};var eo=function(t){return t.mapWithIndex};var Gi={mapWithIndex:vA,Functor0:function(){return $e}};var Mo=function(t){return t.foldrWithIndex};var no=function(t){return t.foldlWithIndex};var ni=function(t){return t.foldMapWithIndex};var Ui=function(t){return t.traverseWithIndex};var ao=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}();var yp=function(t){return function(e){return new ao(e,w(t))}};var Fr=function(){function t(){}return t.value=new t,t}(),ce=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}(),Ap=function(t){return t},Qx=function(t){return new ce(t.value0,t.value1)};var Kx=function(t){var e=function(r){return function(n){var a=r,u=!1,i;function o(m,s){if(s instanceof ce&&s.value1 instanceof ce&&s.value1.value1 instanceof ce){a=new ce(s,m),n=s.value1.value1.value1;return}var l=function(D){return D instanceof ce&&D.value1 instanceof ce&&D.value1.value1 instanceof Fr?new ce(t(D.value0),new ce(t(D.value1.value0),Fr.value)):D instanceof ce&&D.value1 instanceof Fr?new ce(t(D.value0),Fr.value):Fr.value},v=function(D){return function(c){var C=D,ut=!1,mt;function ve(ne,Ze){if(ne instanceof ce&&ne.value0 instanceof ce&&ne.value0.value1 instanceof ce&&ne.value0.value1.value1 instanceof ce){C=ne.value1,c=new ce(t(ne.value0.value0),new ce(t(ne.value0.value1.value0),new ce(t(ne.value0.value1.value1.value0),Ze)));return}return ut=!0,Ze}for(;!ut;)mt=ve(C,c);return mt}};return u=!0,v(m)(l(s))}for(;!u;)i=o(a,n);return i}};return e(Fr.value)},kp={map:Kx};var Ga={foldr:function(t){return function(e){var r=function(){var a=function(u){return function(i){var o=u,m=!1,s;function l(v,D){if(D instanceof Fr)return m=!0,v;if(D instanceof ce){o=new ce(D.value0,v),i=D.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): "+[v.constructor.name,D.constructor.name])}for(;!m;)s=l(o,i);return s}};return a(Fr.value)}(),n=Dr(Ga)(xt(t))(e);return function(a){return n(r(a))}}},foldl:function(t){var e=function(r){return function(n){var a=r,u=!1,i;function o(m,s){if(s instanceof Fr)return u=!0,m;if(s instanceof ce){a=t(m)(s.value0),n=s.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): "+[s.constructor.name])}for(;!u;)i=o(a,n);return i}};return e},foldMap:function(t){return function(e){return Dr(Ga)(function(r){var n=bt(t.Semigroup0())(r);return function(a){return n(e(a))}})(Pt(t))}}};var Pl={append:function(t){return function(e){return er(Ga)(ce.create)(e)(t)}}};var Jm={append:function(t){return function(e){return new ao(t.value0,bt(Pl)(t.value1)(Qx(e)))}}};var yA={alt:bt(Pl),Functor0:function(){return kp}},jm=function(){return{empty:Fr.value,Alt0:function(){return yA}}}();var SA=function(t){return t()};var hA=function(t){throw new Error(t)};var TA=function(){return hA};var bF=SA,Qa=function(t){return bF(function(){return TA()(t)})};var Qt=function(){function t(){}return t.value=new t,t}(),se=function(){function t(e,r,n,a){this.value0=e,this.value1=r,this.value2=n,this.value3=a}return t.create=function(e){return function(r){return function(n){return function(a){return new t(e,r,n,a)}}}},t}(),Ge=function(){function t(e,r,n,a,u,i,o){this.value0=e,this.value1=r,this.value2=n,this.value3=a,this.value4=u,this.value5=i,this.value6=o}return t.create=function(e){return function(r){return function(n){return function(a){return function(u){return function(i){return function(o){return new t(e,r,n,a,u,i,o)}}}}}}},t}(),Hi=function(){function t(e,r,n){this.value0=e,this.value1=r,this.value2=n}return t.create=function(e){return function(r){return function(n){return new t(e,r,n)}}},t}(),fi=function(){function t(e,r,n){this.value0=e,this.value1=r,this.value2=n}return t.create=function(e){return function(r){return function(n){return new t(e,r,n)}}},t}(),zi=function(){function t(e,r,n,a,u,i){this.value0=e,this.value1=r,this.value2=n,this.value3=a,this.value4=u,this.value5=i}return t.create=function(e){return function(r){return function(n){return function(a){return function(u){return function(i){return new t(e,r,n,a,u,i)}}}}}},t}(),wo=function(){function t(e,r,n,a,u,i){this.value0=e,this.value1=r,this.value2=n,this.value3=a,this.value4=u,this.value5=i}return t.create=function(e){return function(r){return function(n){return function(a){return function(u){return function(i){return new t(e,r,n,a,u,i)}}}}}},t}(),Vi=function(){function t(e,r,n,a,u,i){this.value0=e,this.value1=r,this.value2=n,this.value3=a,this.value4=u,this.value5=i}return t.create=function(e){return function(r){return function(n){return function(a){return function(u){return function(i){return new t(e,r,n,a,u,i)}}}}}},t}(),Cp=function(){function t(e,r,n,a){this.value0=e,this.value1=r,this.value2=n,this.value3=a}return t.create=function(e){return function(r){return function(n){return function(a){return new t(e,r,n,a)}}}},t}();var FA=function(t){return function(e){return new se(Qt.value,t,e,Qt.value)}};var EF=function(t){return function(e){var r=ee(t),n=function(a){var u=!1,i;function o(m){if(m instanceof Qt)return u=!0,V.value;if(m instanceof se){var s=r(e)(m.value1);if(s instanceof ye)return u=!0,new B(m.value2);if(s instanceof te){a=m.value0;return}a=m.value3;return}if(m instanceof Ge){var l=r(e)(m.value1);if(l instanceof ye)return u=!0,new B(m.value2);var v=r(e)(m.value4);if(v instanceof ye)return u=!0,new B(m.value5);if(l instanceof te){a=m.value0;return}if(v instanceof be){a=m.value6;return}a=m.value3;return}throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): "+[m.constructor.name])}for(;!u;)i=o(a);return i};return n}};var $A=function(t){return t instanceof Qt};var an=function(t){return function(e){return function(r){var n=t,a=e,u=!1,i;function o(m,s,l){if(s instanceof Fr)return u=!0,l;if(s instanceof ce){if(s.value0 instanceof Hi){n=m,a=s.value1,r=new se(l,s.value0.value0,s.value0.value1,s.value0.value2);return}if(s.value0 instanceof fi){n=m,a=s.value1,r=new se(s.value0.value0,s.value0.value1,s.value0.value2,l);return}if(s.value0 instanceof zi){n=m,a=s.value1,r=new Ge(l,s.value0.value0,s.value0.value1,s.value0.value2,s.value0.value3,s.value0.value4,s.value0.value5);return}if(s.value0 instanceof wo){n=m,a=s.value1,r=new Ge(s.value0.value0,s.value0.value1,s.value0.value2,l,s.value0.value3,s.value0.value4,s.value0.value5);return}if(s.value0 instanceof Vi){n=m,a=s.value1,r=new Ge(s.value0.value0,s.value0.value1,s.value0.value2,s.value0.value3,s.value0.value4,s.value0.value5,l);return}throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): "+[s.value0.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): "+[s.constructor.name,l.constructor.name])}for(;!u;)i=o(n,a,r);return i}}},Rl=function(t){return function(e){return function(r){var n=function(i){return function(o){var m=i,s=!1,l;function v(D,c){if(D instanceof Fr)return s=!0,new se(c.value0,c.value1,c.value2,c.value3);if(D instanceof ce){if(D.value0 instanceof Hi)return s=!0,an(t)(D.value1)(new Ge(c.value0,c.value1,c.value2,c.value3,D.value0.value0,D.value0.value1,D.value0.value2));if(D.value0 instanceof fi)return s=!0,an(t)(D.value1)(new Ge(D.value0.value0,D.value0.value1,D.value0.value2,c.value0,c.value1,c.value2,c.value3));if(D.value0 instanceof zi){m=D.value1,o=new Cp(new se(c.value0,c.value1,c.value2,c.value3),D.value0.value0,D.value0.value1,new se(D.value0.value2,D.value0.value3,D.value0.value4,D.value0.value5));return}if(D.value0 instanceof wo){m=D.value1,o=new Cp(new se(D.value0.value0,D.value0.value1,D.value0.value2,c.value0),c.value1,c.value2,new se(c.value3,D.value0.value3,D.value0.value4,D.value0.value5));return}if(D.value0 instanceof Vi){m=D.value1,o=new Cp(new se(D.value0.value0,D.value0.value1,D.value0.value2,D.value0.value3),D.value0.value4,D.value0.value5,new se(c.value0,c.value1,c.value2,c.value3));return}throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): "+[D.value0.constructor.name,c.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): "+[D.constructor.name,c.constructor.name])}for(;!s;)l=v(m,o);return l}},a=ee(t),u=function(i){return function(o){var m=i,s=!1,l;function v(D,c){if(c instanceof Qt)return s=!0,n(D)(new Cp(Qt.value,e,r,Qt.value));if(c instanceof se){var C=a(e)(c.value1);if(C instanceof ye)return s=!0,an(t)(D)(new se(c.value0,e,r,c.value3));if(C instanceof te){m=new ce(new Hi(c.value1,c.value2,c.value3),D),o=c.value0;return}m=new ce(new fi(c.value0,c.value1,c.value2),D),o=c.value3;return}if(c instanceof Ge){var ut=a(e)(c.value1);if(ut instanceof ye)return s=!0,an(t)(D)(new Ge(c.value0,e,r,c.value3,c.value4,c.value5,c.value6));var mt=a(e)(c.value4);if(mt instanceof ye)return s=!0,an(t)(D)(new Ge(c.value0,c.value1,c.value2,c.value3,e,r,c.value6));if(ut instanceof te){m=new ce(new zi(c.value1,c.value2,c.value3,c.value4,c.value5,c.value6),D),o=c.value0;return}if(ut instanceof be&&mt instanceof te){m=new ce(new wo(c.value0,c.value1,c.value2,c.value4,c.value5,c.value6),D),o=c.value3;return}m=new ce(new Vi(c.value0,c.value1,c.value2,c.value3,c.value4,c.value5),D),o=c.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): "+[D.constructor.name,c.constructor.name])}for(;!s;)l=v(m,o);return l}};return u(Fr.value)}}},SF=function(t){return function(e){var r=function(o){return function(m){var s=o,l=!1,v;function D(c,C){if(c instanceof Fr)return l=!0,C;if(c instanceof ce){if(c.value0 instanceof Hi&&c.value0.value2 instanceof Qt&&C instanceof Qt)return l=!0,an(t)(c.value1)(new se(Qt.value,c.value0.value0,c.value0.value1,Qt.value));if(c.value0 instanceof fi&&c.value0.value0 instanceof Qt&&C instanceof Qt)return l=!0,an(t)(c.value1)(new se(Qt.value,c.value0.value1,c.value0.value2,Qt.value));if(c.value0 instanceof Hi&&c.value0.value2 instanceof se){s=c.value1,m=new Ge(C,c.value0.value0,c.value0.value1,c.value0.value2.value0,c.value0.value2.value1,c.value0.value2.value2,c.value0.value2.value3);return}if(c.value0 instanceof fi&&c.value0.value0 instanceof se){s=c.value1,m=new Ge(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3,c.value0.value1,c.value0.value2,C);return}return c.value0 instanceof Hi&&c.value0.value2 instanceof Ge?(l=!0,an(t)(c.value1)(new se(new se(C,c.value0.value0,c.value0.value1,c.value0.value2.value0),c.value0.value2.value1,c.value0.value2.value2,new se(c.value0.value2.value3,c.value0.value2.value4,c.value0.value2.value5,c.value0.value2.value6)))):c.value0 instanceof fi&&c.value0.value0 instanceof Ge?(l=!0,an(t)(c.value1)(new se(new se(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3),c.value0.value0.value4,c.value0.value0.value5,new se(c.value0.value0.value6,c.value0.value1,c.value0.value2,C)))):c.value0 instanceof zi&&c.value0.value2 instanceof Qt&&c.value0.value5 instanceof Qt&&C instanceof Qt?(l=!0,an(t)(c.value1)(new Ge(Qt.value,c.value0.value0,c.value0.value1,Qt.value,c.value0.value3,c.value0.value4,Qt.value))):c.value0 instanceof wo&&c.value0.value0 instanceof Qt&&c.value0.value5 instanceof Qt&&C instanceof Qt?(l=!0,an(t)(c.value1)(new Ge(Qt.value,c.value0.value1,c.value0.value2,Qt.value,c.value0.value3,c.value0.value4,Qt.value))):c.value0 instanceof Vi&&c.value0.value0 instanceof Qt&&c.value0.value3 instanceof Qt&&C instanceof Qt?(l=!0,an(t)(c.value1)(new Ge(Qt.value,c.value0.value1,c.value0.value2,Qt.value,c.value0.value4,c.value0.value5,Qt.value))):c.value0 instanceof zi&&c.value0.value2 instanceof se?(l=!0,an(t)(c.value1)(new se(new Ge(C,c.value0.value0,c.value0.value1,c.value0.value2.value0,c.value0.value2.value1,c.value0.value2.value2,c.value0.value2.value3),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof wo&&c.value0.value0 instanceof se?(l=!0,an(t)(c.value1)(new se(new Ge(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3,c.value0.value1,c.value0.value2,C),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof wo&&c.value0.value5 instanceof se?(l=!0,an(t)(c.value1)(new se(c.value0.value0,c.value0.value1,c.value0.value2,new Ge(C,c.value0.value3,c.value0.value4,c.value0.value5.value0,c.value0.value5.value1,c.value0.value5.value2,c.value0.value5.value3)))):c.value0 instanceof Vi&&c.value0.value3 instanceof se?(l=!0,an(t)(c.value1)(new se(c.value0.value0,c.value0.value1,c.value0.value2,new Ge(c.value0.value3.value0,c.value0.value3.value1,c.value0.value3.value2,c.value0.value3.value3,c.value0.value4,c.value0.value5,C)))):c.value0 instanceof zi&&c.value0.value2 instanceof Ge?(l=!0,an(t)(c.value1)(new Ge(new se(C,c.value0.value0,c.value0.value1,c.value0.value2.value0),c.value0.value2.value1,c.value0.value2.value2,new se(c.value0.value2.value3,c.value0.value2.value4,c.value0.value2.value5,c.value0.value2.value6),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof wo&&c.value0.value0 instanceof Ge?(l=!0,an(t)(c.value1)(new Ge(new se(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3),c.value0.value0.value4,c.value0.value0.value5,new se(c.value0.value0.value6,c.value0.value1,c.value0.value2,C),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof wo&&c.value0.value5 instanceof Ge?(l=!0,an(t)(c.value1)(new Ge(c.value0.value0,c.value0.value1,c.value0.value2,new se(C,c.value0.value3,c.value0.value4,c.value0.value5.value0),c.value0.value5.value1,c.value0.value5.value2,new se(c.value0.value5.value3,c.value0.value5.value4,c.value0.value5.value5,c.value0.value5.value6)))):c.value0 instanceof Vi&&c.value0.value3 instanceof Ge?(l=!0,an(t)(c.value1)(new Ge(c.value0.value0,c.value0.value1,c.value0.value2,new se(c.value0.value3.value0,c.value0.value3.value1,c.value0.value3.value2,c.value0.value3.value3),c.value0.value3.value4,c.value0.value3.value5,new se(c.value0.value3.value6,c.value0.value4,c.value0.value5,C)))):(l=!0,Qa("The impossible happened in partial function `up`."))}throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): "+[c.constructor.name])}for(;!l;)v=D(s,m);return v}},n=function(o){return function(m){var s=o,l=!1,v;function D(c,C){if(C instanceof se&&C.value0 instanceof Qt&&C.value3 instanceof Qt)return l=!0,r(c)(Qt.value);if(C instanceof se){s=new ce(new fi(C.value0,C.value1,C.value2),c),m=C.value3;return}if(C instanceof Ge&&C.value0 instanceof Qt&&C.value3 instanceof Qt&&C.value6 instanceof Qt)return l=!0,r(new ce(new fi(Qt.value,C.value1,C.value2),c))(Qt.value);if(C instanceof Ge){s=new ce(new Vi(C.value0,C.value1,C.value2,C.value3,C.value4,C.value5),c),m=C.value6;return}return l=!0,Qa("The impossible happened in partial function `removeMaxNode`.")}for(;!l;)v=D(s,m);return v}},a=function(o){var m=!1,s;function l(v){if(v instanceof se&&v.value3 instanceof Qt)return m=!0,{key:v.value1,value:v.value2};if(v instanceof se){o=v.value3;return}if(v instanceof Ge&&v.value6 instanceof Qt)return m=!0,{key:v.value4,value:v.value5};if(v instanceof Ge){o=v.value6;return}return m=!0,Qa("The impossible happened in partial function `maxNode`.")}for(;!m;)s=l(o);return s},u=ee(t),i=function(o){return function(m){var s=o,l=!1,v;function D(c,C){if(C instanceof Qt)return l=!0,V.value;if(C instanceof se){var ut=u(e)(C.value1);if(C.value3 instanceof Qt&&ut instanceof ye)return l=!0,new B(new at(C.value2,r(c)(Qt.value)));if(ut instanceof ye){var mt=a(C.value0);return l=!0,new B(new at(C.value2,n(new ce(new Hi(mt.key,mt.value,C.value3),c))(C.value0)))}if(ut instanceof te){s=new ce(new Hi(C.value1,C.value2,C.value3),c),m=C.value0;return}s=new ce(new fi(C.value0,C.value1,C.value2),c),m=C.value3;return}if(C instanceof Ge){var ve=function(){return C.value0 instanceof Qt&&C.value3 instanceof Qt&&C.value6 instanceof Qt}(),ut=u(e)(C.value4),ne=u(e)(C.value1);if(ve&&ne instanceof ye)return l=!0,new B(new at(C.value2,an(t)(c)(new se(Qt.value,C.value4,C.value5,Qt.value))));if(ve&&ut instanceof ye)return l=!0,new B(new at(C.value5,an(t)(c)(new se(Qt.value,C.value1,C.value2,Qt.value))));if(ne instanceof ye){var mt=a(C.value0);return l=!0,new B(new at(C.value2,n(new ce(new zi(mt.key,mt.value,C.value3,C.value4,C.value5,C.value6),c))(C.value0)))}if(ut instanceof ye){var mt=a(C.value3);return l=!0,new B(new at(C.value5,n(new ce(new wo(C.value0,C.value1,C.value2,mt.key,mt.value,C.value6),c))(C.value3)))}if(ne instanceof te){s=new ce(new zi(C.value1,C.value2,C.value3,C.value4,C.value5,C.value6),c),m=C.value0;return}if(ne instanceof be&&ut instanceof te){s=new ce(new wo(C.value0,C.value1,C.value2,C.value4,C.value5,C.value6),c),m=C.value3;return}s=new ce(new Vi(C.value0,C.value1,C.value2,C.value3,C.value4,C.value5),c),m=C.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): "+[C.constructor.name])}for(;!l;)v=D(s,m);return v}};return i(Fr.value)}},ha={foldr:function(t){return function(e){return function(r){if(r instanceof Qt)return e;if(r instanceof se)return er(ha)(t)(t(r.value2)(er(ha)(t)(e)(r.value3)))(r.value0);if(r instanceof Ge)return er(ha)(t)(t(r.value2)(er(ha)(t)(t(r.value5)(er(ha)(t)(e)(r.value6)))(r.value3)))(r.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): "+[r.constructor.name])}}},foldl:function(t){return function(e){return function(r){if(r instanceof Qt)return e;if(r instanceof se)return Dr(ha)(t)(t(Dr(ha)(t)(e)(r.value0))(r.value2))(r.value3);if(r instanceof Ge)return Dr(ha)(t)(t(Dr(ha)(t)(t(Dr(ha)(t)(e)(r.value0))(r.value2))(r.value3))(r.value5))(r.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): "+[r.constructor.name])}}},foldMap:function(t){return function(e){return function(r){if(r instanceof Qt)return Pt(t);if(r instanceof se)return bt(t.Semigroup0())(xn(ha)(t)(e)(r.value0))(bt(t.Semigroup0())(e(r.value2))(xn(ha)(t)(e)(r.value3)));if(r instanceof Ge)return bt(t.Semigroup0())(xn(ha)(t)(e)(r.value0))(bt(t.Semigroup0())(e(r.value2))(bt(t.Semigroup0())(xn(ha)(t)(e)(r.value3))(bt(t.Semigroup0())(e(r.value5))(xn(ha)(t)(e)(r.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): "+[r.constructor.name])}}}},pa={foldrWithIndex:function(t){return function(e){return function(r){if(r instanceof Qt)return e;if(r instanceof se)return Mo(pa)(t)(t(r.value1)(r.value2)(Mo(pa)(t)(e)(r.value3)))(r.value0);if(r instanceof Ge)return Mo(pa)(t)(t(r.value1)(r.value2)(Mo(pa)(t)(t(r.value4)(r.value5)(Mo(pa)(t)(e)(r.value6)))(r.value3)))(r.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 147, column 26 - line 150, column 120): "+[r.constructor.name])}}},foldlWithIndex:function(t){return function(e){return function(r){if(r instanceof Qt)return e;if(r instanceof se)return no(pa)(t)(t(r.value1)(no(pa)(t)(e)(r.value0))(r.value2))(r.value3);if(r instanceof Ge)return no(pa)(t)(t(r.value4)(no(pa)(t)(t(r.value1)(no(pa)(t)(e)(r.value0))(r.value2))(r.value3))(r.value5))(r.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 151, column 26 - line 154, column 120): "+[r.constructor.name])}}},foldMapWithIndex:function(t){return function(e){return function(r){if(r instanceof Qt)return Pt(t);if(r instanceof se)return bt(t.Semigroup0())(ni(pa)(t)(e)(r.value0))(bt(t.Semigroup0())(e(r.value1)(r.value2))(ni(pa)(t)(e)(r.value3)));if(r instanceof Ge)return bt(t.Semigroup0())(ni(pa)(t)(e)(r.value0))(bt(t.Semigroup0())(e(r.value1)(r.value2))(bt(t.Semigroup0())(ni(pa)(t)(e)(r.value3))(bt(t.Semigroup0())(e(r.value4)(r.value5))(ni(pa)(t)(e)(r.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 155, column 26 - line 158, column 128): "+[r.constructor.name])}}},Foldable0:function(){return ha}},MA=function(){return Mo(pa)(function(t){return function(e){return function(r){return new ce(t,r)}}})(Fr.value)}();var Sp=function(){return Qt.value}();var Zm=function(t){return function(e){return function(r){return Yt(r)(rn)(SF(t)(e)(r))}}};var hp=function(t){return function(e){return function(r){return function(n){var a=e(EF(t)(r)(n));if(a instanceof V)return Zm(t)(r)(n);if(a instanceof B)return Rl(t)(r)(a.value0)(n);throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): "+[a.constructor.name])}}}};var hF=function(t){return function(e){return function(r){return function(n){var a=function(u){return function(i){return function(o){return hp(t)(function(){var m=Yt(o)(e(o));return function(s){return B.create(m(s))}}())(u)(i)}}};return no(pa)(a)(n)(r)}}}};var wA=function(t){return hF(t)(x)};var Ll=function(t){return t.partitionMap};var Ji=function(t){return t.filterMap};var Wl=function(t){return t.filter};var pc=function(t){return{always:tt(et),Monoid0:function(){return t}}};var Bn={dimap:function(t){return function(e){return function(r){return function(n){return e(r(t(n)))}}}}},Po=function(t){return t.dimap},Ka=function(t){return function(e){return Po(t)(e)(tt(et))}};var MF=function(t){return function(e){return function(r){return wA(t)(e)(r)}}};var nv=function(t){return MA(t)};var WA=function(t){return FA(t)(void 0)};var av=function(t){return{append:MF(t)}};var BA=function(t){return $A(t)},GA=function(t){return function(e){return function(r){return Rl(t)(e)(void 0)(r)}}};var UA={foldMap:function(t){return function(e){var r=xn(Ga)(t)(e);return function(n){return r(nv(n))}}},foldl:function(t){return function(e){var r=Dr(Ga)(t)(e);return function(n){return r(nv(n))}}},foldr:function(t){return function(e){var r=er(Ga)(t)(e);return function(n){return r(nv(n))}}}};var uv=Sp;var qA=function(t){return{mempty:uv,Semigroup0:function(){return av(t)}}};var Tp=function(t){return function(e){return function(r){return Zm(t)(e)(r)}}};function HA(t){return function(e){return function(){return setTimeout(e,t)}}}function zA(t){return function(){clearTimeout(t)}}var xp=HA;var PF={eq:function(t){return function(e){return t===e}}},Fp={compare:function(t){return function(e){return ee(Xr)(t)(e)}},Eq0:function(){return PF}};var Gl=zA;var uo=function(t){return t.sampleOn};var $r=function(t){return t.keepLatest},Iu=function(t){return t.fold};var Ul=function(t){return function(e){return function(r){return function(n){return Ji(t.Filterable1())(rn)(Iu(t)(function(a){return function(u){return _(ti)(T(ho))(e(a)(u.value0))}})(r)(new at(n,V.value)))}}}},$p=function(t){return function(e){var r=function(n){return function(a){if(a instanceof V)return new B({now:n,last:V.value});if(a instanceof B)return new B({now:n,last:new B(a.value0.now)});throw new Error("Failed pattern match at FRP.Event.Class (line 56, column 3 - line 56, column 50): "+[n.constructor.name,a.constructor.name])}};return Ji(t.Filterable1())(tt(et))(Iu(t)(r)(e)(V.value))}},ql=function(t){return t.fix};var $n=function(t){return function(e){return function(r){return N(t.Plus0().Alt0())(uo(t)(e)(r))(uo(t)(r)(_(t.Filterable1().Functor1())(Qf)(e)))}}},Y=function(t){return t.bang};function iv(t){return function(e){return t===e}}var fv=iv;var NF=function(t){return t};var $t=function(t){return function(e){return t(e)}},LF=function(t){return function(e){return function(r){return function(n){return function(a){return q(t.Monad0().Bind1())(Me(t)(la(V.value)))(function(u){return q(t.Monad0().Bind1())(r(function(i){return Me(t)(Ae(Ni)(ja(new B(i))(u)))}))(function(i){return q(t.Monad0().Bind1())(n(function(o){return q(t.Monad0().Bind1())(Me(t)(Nn(u)))(vr(e)(Ke)(function(m){return a(o(m))}))}))(function(o){return T(e)(X(e.Apply0())(i)(o))})})})}}}}},Wt=NF,WF=function(t){return function(e){return function(r){return q(t.Monad0().Bind1())(Me(t)(la(V.value)))(function(n){return q(t.Monad0().Bind1())(e(function(a){return ar(je)(t.Monad0().Bind1())(q(t.Monad0().Bind1())(Me(t)(Nn(n)))(lp(t.Monad0().Applicative0())(Ke)))(function(){return q(t.Monad0().Bind1())($t(a)(r))(function(u){return Me(t)(Ae(Ni)(ja(new B(u))(n)))})})}))(function(a){return T(t.Monad0().Applicative0())(ar(je)(t.Monad0().Bind1())(q(t.Monad0().Bind1())(Me(t)(Nn(n)))(lp(t.Monad0().Applicative0())(Ke)))(function(){return a}))})})}}},g={map:function(t){return function(e){return function(r){return e(function(n){return r(t(n))})}}}};var BF=function(t){return function(e){return function(r){return function(n){return function(a){return q(t.Monad0().Bind1())(Me(t)(la(n)))(function(u){return r(function(i){return q(t.Monad0().Bind1())(Me(t)(Pu(e(i))(u)))(a)})})}}}}},Hl=function(t){return function(e){return function(r){return function(n){return r(function(a){var u=e(a);if(u instanceof B)return n(u.value0);if(u instanceof V)return T(t)(void 0);throw new Error("Failed pattern match at FRP.Event (line 126, column 13 - line 128, column 27): "+[u.constructor.name])})}}}},cv=function(t){return function(e){return Hl(t)(function(r){var n=e(r);if(n)return new B(r);if(!n)return V.value;throw new Error("Failed pattern match at FRP.Event (line 84, column 13 - line 86, column 25): "+[n.constructor.name])})}},Ru=function(t){return function(e){return Wt(function(r){return function(){var a=sr(Pt(qA(Fp)))(),u=$t(e)(function(i){return function(){var m=sr(V.value)(),s=xp(t)(function(){r(i)();var v=Rr(m)();return Yt(T(f)(void 0))(function(D){return Af(Tp(Fp)(D))(a)})(v)()})();return Tn(new B(s))(m)(),Af(bt(av(Fp))(WA(s)))(a)()}})();return function(){var o=Rr(a)();return _a(f)(UA)(o)(Gl)(),u()}}})}};var zl=function(t){return function(e){return q(t.Monad0().Bind1())(Me(t)(la([])))(function(r){return T(t.Monad0().Applicative0())({event:function(n){return q(e.Monad0().Bind1())(Me(e)(Pu(function(a){return bt(Sn)(a)([n])})(r)))(function(){return T(e.Monad0().Applicative0())(q(e.Monad0().Bind1())(Me(e)(Pu(zm(fv)(n))(r)))(function(){return T(e.Monad0().Applicative0())(void 0)}))})},push:function(n){return q(e.Monad0().Bind1())(Me(e)(Nn(r)))(vr(e.Monad0().Applicative0())(zt)(function(a){return a(n)}))}})})}},GF=function(t){return function(e){return function(r){return function(n){return q(e.Bind1())(zl(t)(t))(function(a){var u=r(a.event);return q(e.Bind1())($t(u.input)(a.push))(function(i){return q(e.Bind1())($t(u.output)(n))(function(o){return T(e.Applicative0())(X(e.Bind1().Apply0())(i)(o))})})})}}}},ji=function(t){return function(e){return function(r){return Wt(function(n){return q(t.Monad0().Bind1())(zl(t)(t))(function(a){return ar(je)(t.Monad0().Bind1())(n(r(a.event)))(function(){return $t(e)(a.push)})})})}}},jA=function(t){return{compact:Hl(t)(tt(et)),separate:function(e){return{left:Hl(t)(function(r){if(r instanceof jt)return new B(r.value0);if(r instanceof Xt)return V.value;throw new Error("Failed pattern match at FRP.Event (line 67, column 13 - line 69, column 33): "+[r.constructor.name])})(e),right:Hl(t)(function(r){if(r instanceof Xt)return new B(r.value0);if(r instanceof jt)return V.value;throw new Error("Failed pattern match at FRP.Event (line 74, column 13 - line 76, column 32): "+[r.constructor.name])})(e)}}}},bu=function(t){return{filter:cv(t),filterMap:Hl(t),partition:function(e){return function(r){return{yes:cv(t)(e)(r),no:cv(t)(function(){var n=Du(La);return function(a){return n(e(a))}}())(r)}}},partitionMap:function(e){return function(r){return{left:Ji(bu(t))(function(){var n=Ra(B.create)(x(V.value));return function(a){return n(e(a))}}())(r),right:Ji(bu(t))(function(n){return rp(e(n))})(r)}}},Compactable0:function(){return jA(t)},Functor1:function(){return g}}},Gr=function(t){return function(e){return Wt(function(r){return q(t.Monad0().Bind1())(zl(t)(t))(function(n){return ar(je)(t.Monad0().Bind1())(r(e(n.push)(n.event)))(function(){return T(t.Monad0().Applicative0())(T(t.Monad0().Applicative0())(void 0))})})})}},Mt=function(t){return function(e){return function(r){return _(t.Apply0().Functor0())(function(n){return T(t)(void 0)})(r(e))}}},I=function(t){return{alt:function(e){return function(r){return function(n){return Ut(t.Apply0())(_(t.Apply0().Functor0())(function(a){return function(u){return X(t.Apply0())(a)(u)}})(e(n)))(r(n))}}},Functor0:function(){return g}}},S=function(t){return{empty:function(e){return T(t)(T(t)(void 0))},Alt0:function(){return I(t)}}},h=function(t){return{fold:BF(t),keepLatest:WF(t),sampleOn:LF(t)(t.Monad0().Applicative0()),fix:GF(t)(t.Monad0()),bang:Mt(t.Monad0().Applicative0()),Plus0:function(){return S(t.Monad0().Applicative0())},Filterable1:function(){return bu(t.Monad0().Applicative0())}}};var Mp="_____$__$_$$_vbus";function lv(t){return t[Mp]=Mp,t}function _v(t){return()=>{for(let e in t)delete t[e]}}function pv(t){return()=>{let e=(u,i,o,m)=>{let s=Object.keys(m);for(var l=0;l<s.length;l++)if(m[s[l]]instanceof Object&&m[s[l]][Mp]===Mp){let v={},D={};e(u,v,D,m[s[l]]),i[s[l]]=v,o[s[l]]=D}else{let v=`${Math.random()}`;u[v]={},i[s[l]]=D=>()=>{let c=Object.keys(u[v]);for(var C=0;C<c.length;C++)u[v][c[C]](D)()},o[s[l]]=D=>()=>{let c=`${Math.random()}`;return u[v][c]=D,()=>{delete u[v][c]}}}},r={},n={},a={};return e(r,n,a,t),{p:n,e:a,s:r}}}function Vl(t,e){var r={};for(var n in e)({}).hasOwnProperty.call(e,n)&&(r[n]=e[n]);for(var a in t)({}).hasOwnProperty.call(t,a)&&(r[a]=t[a]);return r}var QA=function(t){return function(){return function(){return function(e){return function(r){return function(n){return Vu(Ar(t)(e))(r)(n)}}}}}};var KA=function(){return function(){return function(t){return function(e){return Vl(t,e)}}}},Jl=function(t){return function(){return function(){return function(e){return function(r){return function(n){return Vu(Ar(t)(e))(r)(n)}}}}}},ci=function(t){return function(){return function(e){return function(r){return Oa(Ar(t)(e))(r)}}}};var Gn={vb:function(t){return function(e){return function(r){return{}}}}},wp=function(t){return t.vb},Ya=function(){return function(t){return function(e){return function(r){return function(n){var a=wp(e)(d.value)(d.value)(d.value);return Wt(function(u){return q(t.Monad0().Bind1())(pv(a))(function(i){return ar(je)(t.Monad0().Bind1())(u(n(i.p)(i.e)))(function(){return T(t.Monad0().Applicative0())(_v(i.s))})})})}}}}},Nu=function(t){return function(){return function(){return function(){return function(e){return function(r){return function(){return function(){return function(){return function(){return{vb:function(n){return function(a){return function(u){return Jl(t)()()(d.value)(lv(wp(e)(d.value)(d.value)(d.value)))(wp(r)(d.value)(d.value)(d.value))}}}}}}}}}}}}}},ur=function(t){return function(){return function(){return function(e){return function(){return function(){return function(){return function(){return{vb:function(r){return function(n){return function(a){return Jl(t)()()(d.value)(void 0)(wp(e)(d.value)(d.value)(d.value))}}}}}}}}}}}};var Ki=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),mv=function(){function t(){}return t.value=new t,t}();var sc=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),mc=function(){function t(){}return t.value=new t,t}(),vv=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Pp=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Ef=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Sf=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),O=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var YA=function(t){return t};var Op={eq:function(t){return function(e){return t instanceof Ki&&e instanceof Ki?t.value0===e.value0:t instanceof mv&&e instanceof mv}}};var G=function(t){return new Ef(t)},rt=function(t){return new Sf(t)},Ip=function(t){return new Pp(t)};var Oo=function(t){return t.reflectType};var jl={map:function(t){return function(e){return _($e)(t)(e)}}};var XF=function(t){return Oo(t)},hf=function(){return function(t){return t}};var rk=function(t){return[t]};var nk=function(){return function(){return function(){return function(){return function(){return function(t){return function(e){return function(r){return r[XF(t)(e)]}}}}}}}};var Dv=[];var Tf=function(){return function(){return function(t){return function(e){return Bi(t)(e)}}}};function ak(t){return function(){var e={};for(var r in t)hasOwnProperty.call(t,r)&&(e[r]=t[r]);return e}}var Io={};function dv(t){return t()}function uk(t,e){var r={};for(var n in t)hasOwnProperty.call(t,n)&&(r[n]=e(t[n]));return r}function ok(t,e){var r={};for(var n in t)hasOwnProperty.call(t,n)&&(r[n]=e(n)(t[n]));return r}function ik(t){return function(e){return function(r){return function(n){var a=r;function u(o){return function(m){return e(m)(o)(n[o])}}for(var i in n)hasOwnProperty.call(n,i)&&(a=t(a)(u(i)));return a}}}}function Xl(t){return function(e){var r=[];for(var n in e)hasOwnProperty.call(e,n)&&r.push(t(n)(e[n]));return r}}var QF=Object.keys||Xl(function(t){return function(){return t}});function bv(t){return function(e){return function(r){return function(){return r[t]=e,r}}}}var yv=function(t){return function(e){return function(){return delete e[t],e}}};var Av=Xl(function(t){return function(e){return e}});var n$=ak;var ck=function(t){return function(e){return dv(function(){var n=n$(e)();return t(n)(),n})}};var lk=function(t){return function(e){return ok(e,t)}};var _i=function(t){return function(e){return ck(bv(t)(e))}},Np={map:function(t){return function(e){return uk(e,t)}}},a$={mapWithIndex:lk,Functor0:function(){return Np}},kv=function(){return lt};var Lp=ik(Qf),_k=function(t){return function(e){return Lp(function(r){return function(n){return function(a){return bt(t.Semigroup0())(r)(e(n)(a))}}})(Pt(t))}},Ql={foldl:function(t){return Lp(function(e){return function(r){return t(e)}})},foldr:function(t){return function(e){return function(r){return er(zt)(t)(e)(Av(r))}}},foldMap:function(t){return function(e){return _k(t)(x(e))}}},pk={foldlWithIndex:function(t){return Lp(xt(t))},foldrWithIndex:function(t){return function(e){return function(r){return er(zt)(ac(t))(e)(Xl(at.create)(r))}}},foldMapWithIndex:function(t){return _k(t)},Foldable0:function(){return Ql}},u$={traverseWithIndex:function(t){return function(e){return function(r){return Lp(function(n){return function(a){return function(u){return Ut(t.Apply0())(_(t.Apply0().Functor0())(xt(_i(a)))(n))(e(a)(u))}}})(T(t)(Io))(r)}}},FunctorWithIndex0:function(){return a$},FoldableWithIndex1:function(){return pk},Traversable2:function(){return vc}},vc={traverse:function(t){var e=Ui(u$)(t);return function(r){return e(x(r))}},sequence:function(t){return Wn(vc)(t)(tt(et))},Functor0:function(){return Np},Foldable1:function(){return Ql}};var gv=function(t){return ck(yv(t))};var sk=function(){function t(){}return t.value=new t,t}(),Cv=function(){function t(){}return t.value=new t,t}(),o$=function(){function t(){}return t.value=new t,t}();var Ev=function(t){return function(e){return function(r){var n=function(a){var u=function(i){return function(o){return new at(o+1|0,new at(i,o))}};return Ul(h(t))(u)(a)(0)};return new Pp($r(h(t))(ji(t)(n(r))(function(a){return _(g)(function(u){return N(I(t.Monad0().Applicative0()))(Y(h(t))(new sc(e(u.value0))))(_(g)(x(mc.value))(Wl(bu(t.Monad0().Applicative0()))(function(){var i=Zt(Oi)(u.value1+1|0);return function(o){return i(rn(o))}}())(a)))})(a)})))}}};var oo=function(t){return function(e){return function(r){return function(n){return function(a){var u=function(i){return i(n)(a)};return function(i){if(i instanceof Ef)return sn(zt)(S(t))(oo(t)(e)(r)(n)(a))(i.value0);if(i instanceof Sf)return $r(h(e))(_(g)(oo(t)(e)(r)(n)(a))(i.value0));if(i instanceof O)return u(r.toElt(i.value0));if(i instanceof Pp)return Wt(function(o){return q(e.Monad0().Bind1())(Me(e)(la(Io)))(function(m){return q(e.Monad0().Bind1())($t(i.value0)(function(s){return q(e.Monad0().Bind1())(r.ids(a))(function(l){return q(e.Monad0().Bind1())(Me(e)(la(T(t)(void 0))))(function(v){return q(e.Monad0().Bind1())(r.ids(a))(function(D){return q(e.Monad0().Bind1())(Me(e)(la(T(t)(void 0))))(function(c){return q(e.Monad0().Bind1())(Me(e)(la([])))(function(C){return q(e.Monad0().Bind1())(Me(e)(la(T(t)(void 0))))(function(ut){return q(e.Monad0().Bind1())(_(t.Apply0().Functor0())(Ki.create)(r.ids(a)))(function(mt){return q(e.Monad0().Bind1())(Me(e)(la(sk.value)))(function(ve){return q(e.Monad0().Bind1())($t(s)(function(ne){return q(e.Monad0().Bind1())(Me(e)(Nn(ve)))(function(Ze){return ne instanceof vv&&Ze instanceof Cv?q(e.Monad0().Bind1())(Me(e)(Nn(C)))(vr(t)(zt)(function(){var lr=r.doLogic(ne.value0)(a);return function(pt){return o(lr(pt))}}())):ne instanceof mc&&Ze instanceof Cv?ar(je)(e.Monad0().Bind1())(Ae(t.Apply0().Functor0())(Me(e)(ja(o$.value)(ve))))(function(){var lr=X(t.Apply0())(X(t.Apply0())(X(t.Apply0())(X(t.Apply0())(q(e.Monad0().Bind1())(Me(e)(Nn(C)))(vr(t)(zt)(function(pt){return _a(t)(Ke)(n.parent)(function(De){return o(r.disconnectElement(a)({id:pt,parent:De,scope:mt}))})})))(Ja(e.Monad0().Bind1())(Me(e)(Nn(v)))))(Ja(e.Monad0().Bind1())(Me(e)(Nn(c)))))(Ae(t.Apply0().Functor0())(Me(e)(Pu(gv(l))(m)))))(Ae(t.Apply0().Functor0())(Me(e)(Pu(gv(D))(m))));return X(t.Apply0())(Ae(t.Apply0().Functor0())(Me(e)(ja(lr)(ut))))(lr)}):ne instanceof sc&&Ze instanceof sk?ar(je)(e.Monad0().Bind1())(Ae(t.Apply0().Functor0())(Me(e)(ja(Cv.value)(ve))))(function(){return q(e.Monad0().Bind1())($t(oo(t)(e)(r)({parent:n.parent,scope:mt,raiseId:function(lr){return Ae(t.Apply0().Functor0())(Me(e)(Pu(bt(Sn)([lr]))(C)))}})(a)(ne.value0))(o))(function(lr){return ar(je)(e.Monad0().Bind1())(Ae(t.Apply0().Functor0())(Me(e)(Pu(_i(D)(lr))(m))))(function(){return Ae(t.Apply0().Functor0())(Me(e)(ja(lr)(c)))})})}):T(t)(void 0)})}))(function(ne){return ar(je)(e.Monad0().Bind1())(Ae(t.Apply0().Functor0())(Me(e)(ja(ne)(v))))(function(){return ar(je)(e.Monad0().Bind1())(Ae(t.Apply0().Functor0())(Me(e)(Pu(_i(l)(ne))(m))))(function(){return Ja(e.Monad0().Bind1())(Me(e)(Nn(ut)))})})})})})})})})})})})}))(function(s){return T(t)(ar(je)(e.Monad0().Bind1())(q(e.Monad0().Bind1())(Me(e)(Nn(m)))(Dr(Ql)(X(t.Apply0()))(T(t)(void 0))))(function(){return s}))})})});throw new Error("Failed pattern match at Bolson.Control (line 521, column 17 - line 604, column 20): "+[i.constructor.name])}}}}}},i$=function(){return function(t){return function(e){return function(r){return function(n){return function(a){return function(u){return function(i){var o=function(m){return function(s){return Wt(function(l){return q(t.Monad0().Bind1())(Hb(_($e)(x(""))(hf()(u))))(function(v){var D=Sr(zt)(S(t.Monad0().Applicative0()))(eo(Gi)(function(c){return Cm(Iy)(function(C){return function(ut){return ut instanceof O?function(mt){return mt({parent:V.value,scope:r(m.scope),raiseId:function(ve){return zb(c)(ve)(v)}})(s)}(a.toElt(ut.value0)):C(a.wrapElt(ut))}})})(hf()(u)));return q(t.Monad0().Bind1())($t(D)(l))(function(c){return q(t.Monad0().Bind1())(Me(t)(la(T(t.Monad0().Applicative0())(void 0))))(function(C){return q(t.Monad0().Bind1())(_(t.Monad0().Bind1().Apply0().Functor0())(lt)(Vb(v)))(function(ut){var mt=_(jl)(function(ne){return function(Ze){return new O(a.fromEltO1(function(lr){return function(pt){return Wt(function(De){return ar(je)(t.Monad0().Bind1())(lr.raiseId(ne))(function(){return ar(je)(t.Monad0().Bind1())(_a(t.Monad0().Applicative0())(Ke)(lr.parent)(function(he){return De(a.giveNewParent(pt)({id:ne,parent:he,scope:lr.scope})(Ze))}))(function(){return T(t.Monad0().Applicative0())(T(t.Monad0().Applicative0())(void 0))})})})}}))}})(ut),ve=oo(t.Monad0().Applicative0())(t)(n)(m)(s)(i(mt)(lt));return q(t.Monad0().Bind1())($t(ve)(l))(function(ne){return ar(je)(t.Monad0().Bind1())(Ae(t.Monad0().Bind1().Apply0().Functor0())(Me(t)(ja(ne)(C))))(function(){return T(t.Monad0().Applicative0())(ar(je)(t.Monad0().Bind1())(c)(function(){return ar(je)(t.Monad0().Bind1())(Rn(t.Monad0().Applicative0())(!e)(_a(t.Monad0().Applicative0())(zt)(hf()(ut))(function(Ze){return l(a.deleteFromCache(s)({id:Ze}))})))(function(){return Ja(t.Monad0().Bind1())(Me(t)(Nn(C)))})}))})})})})})})})}};return new O(a.fromEltO2(o))}}}}}}}};var Sv=function(){return function(t){return function(e){return function(r){return function(n){return function(a){return i$()(t)(!1)(tt(et))(e)(r)(n)(a)}}}}}};var mk=function(t){return function(e){return function(r){return function(n){var a=function(u){return function(i){return Wt(function(o){return q(t.Monad0().Bind1())(Me(t)(la(V.value)))(function(m){var s=n(new O(r.fromElt(function(l){return function(v){return Wt(function(D){return ar(je)(t.Monad0().Bind1())(q(t.Monad0().Bind1())(Me(t)(Nn(m)))(function(c){if(c instanceof V)return T(t.Monad0().Applicative0())(void 0);if(c instanceof B)return _a(t.Monad0().Applicative0())(Ke)(l.parent)(function(C){return Rn(t.Monad0().Applicative0())(c.value0!==C)(X(t.Monad0().Bind1().Apply0())(l.raiseId(c.value0))(D(r.connectToParent(i)({id:c.value0,parent:C}))))});throw new Error("Failed pattern match at Bolson.Control (line 630, column 36 - line 637, column 16): "+[c.constructor.name])}))(function(){return T(t.Monad0().Applicative0())(T(t.Monad0().Applicative0())(void 0))})})}})));return $t(oo(t.Monad0().Applicative0())(t)(e)({parent:u.parent,scope:u.scope,raiseId:function(l){return ar(je)(t.Monad0().Bind1())(u.raiseId(l))(function(){return Ae(t.Monad0().Bind1().Apply0().Functor0())(Me(t)(ja(new B(l))(m)))})}})(i)(s))(o)})})}};return new O(r.fromElt(a))}}}};var f$=function(t){return t},Kl=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Yl=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),c$=function(t){return t},Bp=Ln(),b=c$;var U=function(){return Kl.create}();var ct=function(){return Yl.create}(),Ye=function(){var t=_(fa)(_(F)(x(!0)));return function(e){return f$(t(e))}}(),Z=function(t){return t.attr};var _$=function(t){return t.makeText},p$=function(t){return function(e){return function(r){return _(g)(function(n){return t.setText(function(a){return{id:e,text:a}}(n))})(r)}}},s$=function(t){return function(e){return function(r){return _(g)(function(n){return function(a){if(a.value instanceof Kl)return t.setProp({id:e,key:a.key,value:a.value.value0});if(a.value instanceof Yl)return t.setCb({id:e,key:a.key,value:a.value.value0});throw new Error("Failed pattern match at Deku.Control (line 75, column 26 - line 77, column 45): "+[a.value.constructor.name])}(Bp(n))})(r)}}},m$=function(t){return t.makeElement},v$=function(t){return t.attributeParent},un=function(t){return function(e){var r=function(n){return function(a){return Wt(function(u){return q(t.Bind1())(a.ids)(function(i){return ar(je)(t.Bind1())(n.raiseId(i))(function(){return _(t.Bind1().Apply0().Functor0())(X(t.Bind1().Apply0())(u(a.deleteFromCache({id:i}))))($t(Sr(zt)(S(t.Applicative0()))([Mt(t.Applicative0())(_$(a)({id:i,parent:n.parent,scope:n.scope})),p$(a)(i)(e)]))(u))})})})}};return new O(r)}},or=function(t){return function(e){return un(t)(Mt(t.Applicative0())(e))}},vk=function(t){return oo(t.MonadST5().Monad0().Applicative0())(t.MonadST5())({doLogic:function(e){return function(r){return function(n){return r.sendToTop({id:n})}}},ids:function(){var e=Cr();return function(r){return function(n){return n.ids}(e(r))}}(),disconnectElement:function(e){return function(r){return e.disconnectElement({id:r.id,scope:r.scope,parent:r.parent,scopeEq:Zt(Op)})}},toElt:function(e){return e}})},D$=function(t){return function(e){return function(r){return function(n){return Wt(function(a){return q(t.MonadST5().Monad0().Bind1())(n.ids)(function(u){return $t(N(I(t.MonadST5().Monad0().Applicative0()))(Mt(t.MonadST5().Monad0().Applicative0())(n.makeRoot({id:u,root:e})))(vk(t)({parent:new B(u),scope:new Ki("rootScope"),raiseId:function(i){return T(t.MonadST5().Monad0().Applicative0())(void 0)}})(n)(r)))(a)})})}}}};var Dk=function(t){return function(e){return function(r){return D$(t)(e)(new Ef(r))}}},j=function(t){return function(e){return function(r){return function(n){var a=function(u){return function(i){return Wt(function(o){return q(t.MonadST5().Monad0().Bind1())(i.ids)(function(m){return ar(je)(t.MonadST5().Monad0().Bind1())(u.raiseId(m))(function(){return _(t.MonadST5().Monad0().Bind1().Apply0().Functor0())(X(t.MonadST5().Monad0().Bind1().Apply0())(o(i.deleteFromCache({id:m}))))($t(N(I(t.MonadST5().Monad0().Applicative0()))(Sr(zt)(S(t.MonadST5().Monad0().Applicative0()))(bt(Sn)([Mt(t.MonadST5().Monad0().Applicative0())(m$(i)({id:m,parent:u.parent,scope:u.scope,tag:e})),s$(i)(m)(r)])(Yt([])(function(s){return[Mt(t.MonadST5().Monad0().Applicative0())(v$(i)({id:m,parent:s}))]})(u.parent))))(vk(t)({parent:new B(m),scope:u.scope,raiseId:function(s){return T(t.MonadST5().Monad0().Applicative0())(void 0)}})(i)(n)))(o))})})})}};return a}}}};var E$=function(){return function(){return function(){return function(t){return function(e){return function(r){return ll(r.type)(t)?Oa(r.type)(t)(r.value):e(r)}}}}}};var ir=function(){return function(t){return function(e){return function(r){return{type:Ar(t)(e),value:r}}}}};var Ak=function(t){return Qa("Data.Variant: pattern match failure ["+(t.type+"]"))},Ur=function(){return function(){return function(){return function(t){return E$()()()(t)(Ak)}}}};function kk(t){var e={};for(var r in t)({}).hasOwnProperty.call(t,r)&&(e[r]=t[r]);return e}function gk(t){return function(e){return function(r){return r[t]=e,r}}}var Gp=Yo;var Tv=function(){return function(){return function(t){return function(e){return function(r){return function(n){return gk(Ar(t)(e))(r)(n)}}}}}};var Up=et,xv=function(t){return function(e){return t(kk(e))}},Ck=xt(xv)({});var p={Always0:function(){return pc(gr(Ir))},Always1:function(){return pc(gr(gr(Ir)))},Always2:function(){return pc(gr(Ir))},Always3:function(){return pc(xl(et))},Always4:function(){return pc(xl(et))},MonadST5:function(){return k}};var mr=function(){function t(){}return t.value=new t,t}();var dr={attr:function(t){return function(e){return b({key:"click",value:ct(e)})}}};var Vt=function(){function t(){}return t.value=new t,t}();var qp={attr:function(t){return function(e){return b({key:"style",value:U(e)})}}};var Sk={attr:function(t){return function(e){return b({key:"style",value:U(e)})}}};var st={attr:function(t){return function(e){return b({key:"style",value:U(e)})}}};var hk={attr:function(t){return function(e){return b({key:"style",value:U(e)})}}},Dc={attr:function(t){return function(e){return b({key:"style",value:U(e)})}}};var $v={attr:function(t){return function(e){return b({key:"style",value:U(e)})}}};var Tk={attr:function(t){return function(e){return b({key:"style",value:U(e)})}}};var Mv=function(t){return function(e){return function(r){return new O(j(t)("a")(e)(G(r)))}}};var Te=function(t){return function(e){return function(r){return new O(j(t)("div")(e)(G(r)))}}},Ue=function(t){return Te(t)(w(S(t.MonadST5().Monad0().Applicative0())))};var bc=function(t){return function(e){return function(r){return new O(j(t)("span")(e)(G(r)))}}},wv=function(t){return bc(t)(w(S(t.MonadST5().Monad0().Applicative0())))};var Fk=(t,e,r,n)=>{t(a=>n.units[a].main.appendChild(n.units[e].main))(r)},$k=t=>e=>()=>{e.units[t.id].main.parentNode||e.units[t.parent].main.appendChild(e.units[t.id].main)},Mk=t=>e=>r=>()=>{var n,a=e.id;r.scopes[e.scope]||(r.scopes[e.scope]=[]),r.scopes[e.scope].push(a),t&&e.parent.value0&&(n=document.body.querySelectorAll("[data-deku-ssr-"+a+"]").item(0))?r.units[a]={listeners:{},parent:e.parent,scope:e.scope,main:n}:r.units[a]={listeners:{},parent:e.parent,scope:e.scope,main:document.createElement(e.tag)}},wk=t=>e=>r=>n=>()=>{var a=r.id,u;n.scopes[r.scope]||(n.scopes[r.scope]=[]),n.scopes[r.scope].push(a),t&&r.parent.value0&&(u=document.body.querySelectorAll("[data-deku-ssr-"+r.parent.value0+"]").item(0))?n.units[a]={main:u.childNodes[0],parent:r.parent,scope:r.scope}:(n.units[a]={main:document.createTextNode(""),parent:r.parent,scope:r.scope},Fk(e,a,r.parent,n))};function Pv(){return{units:{},scopes:{}}}var Pk=t=>e=>r=>()=>{var n=e.id,a=e.value;t&&!r.units[n]&&(dom=document.body.querySelectorAll("[data-deku-ssr-"+n+"]").item(0))&&(r.units[n]={listeners:{},parent:e.parent,scope:e.scope,main:dom},r.scopes[e.scope]||(r.scopes[e.scope]=[]),r.scopes[e.scope].push(n)),r.units[n].main.tagName==="INPUT"&&e.key==="value"?r.units[n].main.value=a:r.units[n].main.tagName==="INPUT"&&e.key==="checked"?r.units[n].main.checked=a==="true":r.units[n].main.setAttribute(e.key,a)},Ok=t=>e=>r=>()=>{var n=e.id,a=e.value;if(t&&!r.units[n]&&(dom=document.body.querySelectorAll("[data-deku-ssr-"+n+"]").item(0))&&(r.units[n]={listeners:{},parent:e.parent,scope:e.scope,main:dom},r.scopes[e.scope]||(r.scopes[e.scope]=[]),r.scopes[e.scope].push(n)),e.key==="@self@")a(r.units[n].main)();else{r.units[n].listeners[e.key]&&r.units[n].main.removeEventListener(e.key,r.units[n].listeners[e.key]);var u=i=>a(i)();r.units[n].main.addEventListener(e.key,u),r.units[n].listeners[e.key]=u}},Ik=t=>e=>()=>{var r=t.id;e.units[r].main.nodeValue=t.text},Rk=t=>e=>r=>n=>()=>{var a,u,i=r.id,o=r.html,m=r.verb,s=r.cache,l=r.parent,v=r.scope,D=r.pxScope;if(t&&r.parent.value0&&(a=document.body.querySelectorAll("[data-deku-ssr-"+i+"]").item(0)))n.units[i]={listeners:{},scope:v,parent:l,main:a};else{let C=Object.entries(s);for(var c=0;c<C.length;c++){let ut=C[c][0];C[c][1]===!0?o=o.replace(m+ut+m,'data-deku-attr-internal="'+ut+'"'):o=o.replace(m+ut+m,'<span style="display:contents;" data-deku-elt-internal="'+ut+'"></span>')}u=document.createElement("div"),u.innerHTML=o.trim(),n.units[i]={listeners:{},scope:v,parent:l,main:u.firstChild}}n.scopes[v]||(n.scopes[v]=[]),n.scopes[v].push(i),u||(u=a),u.querySelectorAll("[data-deku-attr-internal]").forEach(function(C){var ut=C.getAttribute("data-deku-attr-internal");let mt=ut+D;n.units[mt]={listeners:{},main:C,scope:v},n.scopes[v].push(mt)}),u.querySelectorAll("[data-deku-elt-internal]").forEach(function(C){var ut=C.getAttribute("data-deku-elt-internal");let mt=ut+D;n.units[ut+D]={listeners:{},main:C,scope:v},n.scopes[v].push(mt)}),a||Fk(e,i,l,n)},Nk=t=>e=>()=>{var r=t.id;e.units[r]={main:t.root}},Lk=t=>e=>()=>{var r=t.id,n=t.parent;e.units[r].containingScope=t.scope,e.units[n].main.prepend(e.units[r].main)},Wk=t=>e=>()=>{var r=t.id;e.units[r].noop||e.units[r].containingScope&&!t.scopeEq(e.units[r].containingScope)(t.scope)||e.units[r].main.remove()},Bk=t=>e=>()=>{delete e.units[t.id]},Gk=t=>e=>()=>{var r=t.id;e.units[r].main.parentNode.prepend(e.units[r].main)};var Uk=function(t){return function(e){return function(r){return(r|0)===r?t(r):e}}},ze=function(t){return t};var Ov=function(t){return function(e){return Math.pow(t,e)|0}};var Hp=isFinite;var Zl=Math.floor;var Yi=function(t){return function(e){return Math.pow(t,e)}},t_=function(t){return function(e){return t%e}},zp=Math.round;var Vp=Math.sin;var Zi=3.141592653589793;var yc=function(){return Uk(B.create)(V.value)}(),Hk=function(t){if(!Hp(t))return 0;if(t>=ze(Kn(df)))return Kn(df);if(t<=ze(Yn(df)))return Yn(df);if(nr)return Ea(0)(yc(t));throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): "+[t.constructor.name])},zk=function(t){return Hk(zp(t))};var e_=function(t){return Hk(Zl(t))};var Bu=Math.random;var r_=function(t){return function(e){return function(){var n=Bu(),a=(ze(e)-ze(t)+1)*n+ze(t);return e_(a)}}};var Vk=function(t){return t};var I$=1,Jp=2147483647,R$=function(){return Jp-1|0}(),kc=function(t){var e=function(r){return function(n){return function(a){var u=n-r|0,i=su(xo)(a)(u),o=i<r;return o?i+n|0:i}}};return e(I$)(R$)(t)};var N$=0,L$=48271,Jk=function(t){return function(e){return ta()(yc(t_(ze(L$)*ze(e)+ze(t))(ze(Jp))))}},jk=Jk(N$);var V$=function(){function t(i){this.fn=i}var e={},r=function(i,o){this.head=i,this.tail=o};function n(i){return new r(i,e)}function a(i){return function(o){return new r(i,o)}}function u(i){for(var o=[],m=i;m!==e;)o.push(m.head),m=m.tail;return o}return function(i){return function(o){return function(m){var s=function(v,D){return i(o(a)(m(v)))(D)},l=function(v,D,c){if(D===0)return v;var C=c[D-1];return new t(function(){var ut=l(s(C,v),D-1,c);return ut})};return function(v){for(var D=o(n)(m(v[v.length-1])),c=l(D,v.length-1,v);c instanceof t;)c=c.fn();return o(u)(c)}}}}}();var Zk=function(t){return t};var tg=Sn;var eg=zt;var ag=Zk,a_=function(t){return t};var u_=function(t){return ag(sA(t))};var gc=function(t){if(Ou(t)>0)return new B(ag(t));if(nr)return V.value;throw new Error("Failed pattern match at Data.Array.NonEmpty (line 157, column 1 - line 157, column 58): "+[t.constructor.name])};var ug=function(t){return function(e){return t(a_(e))}};var og=ug(Ou);var ig=function(){return ug(qm())};var pi=function(t){return t.state};function Lo(t){return new Error(t)}function $f(t){return function(){throw t}}function Qp(t){return function(e){return function(){try{return e()}catch(r){return r instanceof Error||Object.prototype.toString.call(r)==="[object Error]"?t(r)():t(new Error(r.toString()))()}}}}var fo=function(t){return t.throwError};var SM={throwError:$f,Monad0:function(){return ue}};var Vv={catchError:xt(Qp),MonadThrow0:function(){return SM}};var si=function(t){return t.catchError};var i_=function(t){return function(e){return si(t)(_(t.MonadThrow0().Monad0().Bind1().Apply0().Functor0())(Xt.create)(e))(function(){var r=T(t.MonadThrow0().Monad0().Applicative0());return function(n){return r(jt.create(n))}}())}};var fr={liftEffect:tt(et),Monad0:function(){return ue}},br=function(t){return t.liftEffect};var jv=function(t){return{map:function(e){return function(r){return function(n){return _(t)(function(a){return new at(e(a.value0),a.value1)})(r(n))}}}}};var Xv=function(t){return{Applicative0:function(){return Kv(t)},Bind1:function(){return Qv(t)}}},Qv=function(t){return{bind:function(e){return function(r){return function(n){return q(t.Bind1())(e(n))(function(a){var u=r(a.value0);return u(a.value1)})}}},Apply0:function(){return es(t)}}},es=function(t){return{apply:ju(Xv(t)),Functor0:function(){return jv(t.Bind1().Apply0().Functor0())}}},Kv=function(t){return{pure:function(e){return function(r){return T(t.Applicative0())(new at(e,r))}},Apply0:function(){return es(t)}}};var mg=function(t){return{state:function(e){var r=T(t.Applicative0());return function(n){return r(e(n))}},Monad0:function(){return Xv(t)}}};var Dg=function(t){return function(e){var r=t(e);return r.value0}};var PM=function(t){return t};var bg=function(){var t=function(e){return new at(Vk(e.newSeed),function(){var r={};for(var n in e)({}).hasOwnProperty.call(e,n)&&(r[n]=e[n]);return r.newSeed=jk(e.newSeed),r}())};return pi(mg(Qu))(t)}();var co=jv(To),yg=_(co)(function(t){return ze(t)/ze(Jp)})(bg);var Sc=function(t){return Dg(PM(t))};var wf=Qv(Qu);var Pf=es(Qu),dg=function(t){return function(e){var r=ze(e),n=ze(t),a=function(o){return n+t_(o)(r-n+1)},u=_(co)(ze)(bg),i=Ut(Pf)(_(co)(We(ga))(u))(_(co)(In(ga)(2))(u));return _(co)(function(o){return e_(a(o))})(i)}},Yv=function(t){return function(e){var r=t<=e;return r?dg(t)(e):dg(e)(t)}};var p_=Kv(Qu);var Zv=function(t){return q(wf)(Yv(0)(og(t)-1|0))(function(e){return T(p_)(ig()(t)(e))})};var m_=function(t){return t.arbitrary};var Ag={arbitrary:yg};var kg=function(){return{arbitrary:Yv(-1e6)(1e6)}}();var gg=function(t){return{ids:function(){var r=Rr(t)(),n=Ht(Xa)(Sc(m_(kg))({newSeed:kc(r),size:5}));return Ae(F)(tc(We(Fu)(1))(t))(),n},makeElement:Mk(!1),attributeParent:$k,makeRoot:Nk,makeText:wk(!1)(Yt(void 0)),makePursx:Rk(!1)(Yt(void 0)),setProp:Pk(!1),setCb:Ok(!1),setText:Ik,sendToTop:Gk,deleteFromCache:Bk,giveNewParent:Lk,disconnectElement:Wk}};var WM=function(t){return t};var K=function(t){return{pursxToElement:function(e){return function(r){return function(n){return{cache:Io,element:function(a){return function(u){return w(S(t))}}}}}}}},eD=function(t){return t.pursxToElement},on=function(){return function(t){return function(e){return function(r){return function(n){return{pursxToElement:function(a){return function(u){return function(i){var o=eD(t)(a)(d.value)(i);return{cache:_i(Oo(e)(d.value))(!0)(o.cache),element:function(m){return function(s){return N(I(n.MonadST5().Monad0().Applicative0()))(_(g)(Ka(Bn)(Bp)(function(l){if(l.value instanceof Kl)return s.setProp({id:Oo(e)(d.value)+a,key:l.key,value:l.value.value0});if(l.value instanceof Yl)return s.setCb({id:Oo(e)(d.value)+a,key:l.key,value:l.value.value0});throw new Error("Failed pattern match at Deku.Pursx (line 4474, column 38 - line 4484, column 24): "+[l.value.constructor.name])}))(ci(r)()(d.value)(i)))(o.element(m)(s))}}}}}}}}}}}};var L=WM,pe=function(t){return function(e){return function(){return function(){return function(r){return function(n){return function(a){return function(u){return function(i){var o=function(m){return function(s){return Wt(function(l){return q(r.MonadST5().Monad0().Bind1())(s.ids)(function(v){return q(r.MonadST5().Monad0().Bind1())(s.ids)(function(D){return ar(je)(r.MonadST5().Monad0().Bind1())(m.raiseId(v))(function(){var c=eD(n)(D)(d.value)(i);return _(r.MonadST5().Monad0().Bind1().Apply0().Functor0())(X(r.MonadST5().Monad0().Bind1().Apply0())(l(s.deleteFromCache({id:v}))))($t(N(I(r.MonadST5().Monad0().Applicative0()))(Mt(r.MonadST5().Monad0().Applicative0())(s.makePursx({id:v,parent:m.parent,cache:c.cache,pxScope:D,scope:m.scope,html:Oo(t)(u),verb:Oo(e)(a)})))(c.element(m)(s)))(l))})})})})}};return new O(o)}}}}}}}}},wt=function(t){return function(){return function(){return function(e){return function(r){return pe(t)({reflectType:function(){return"~"}})()()(r)(e)(d.value)}}}}};var BM=function(t){return oo(t.MonadST5().Monad0().Applicative0())(t.MonadST5())({doLogic:function(e){return function(r){return function(n){return r.sendToTop({id:n})}}},ids:function(){var e=Cr();return function(r){return function(n){return n.ids}(e(r))}}(),disconnectElement:function(e){return function(r){return e.disconnectElement({id:r.id,scope:r.scope,parent:r.parent,scopeEq:Zt(Op)})}},toElt:function(e){return e}})},W=function(){return function(t){return function(e){return function(r){return function(n){return{pursxToElement:function(a){return function(u){return function(i){var o=ci(r)()(d.value)(i),m=eD(t)(a)(d.value)(i);return{cache:_i(Oo(e)(d.value))(!1)(m.cache),element:function(s){return function(l){return N(I(n.MonadST5().Monad0().Applicative0()))(BM(n)({parent:new B(Oo(e)(d.value)+a),scope:s.scope,raiseId:function(v){return T(n.MonadST5().Monad0().Applicative0())(void 0)}})(l)(o))(m.element(s)(l))}}}}}}}}}}}};var At=function(){return function(){return{defaults:xt(KA()())}}},GM=function(t){return t.defaults},kt={convertRecordOptions:function(t){return function(e){return function(r){return tt(Up)}}}},Cg=function(t){return t.convertRecordOptions},sa=function(t){return t.convertOptionsWithDefaults},gt=function(){return function(t){return{convertOptions:function(e){return function(r){return Ck(Cg(t)(e)(d.value)(r))}}}}},UM=function(t){return t.convertOptions},Ct=function(t){return function(e){return{convertOptionsWithDefaults:function(r){return function(n){var a=GM(e)(n),u=UM(t)(r);return function(i){return a(u(i))}}}}}},qM=function(t){return t.convertOption},J=function(t){return function(e){return function(){return function(){return function(){return function(r){return{convertRecordOptions:function(n){return function(a){return function(u){return wu(Gp)(Tv()()(r)(d.value)(qM(e)(n)(d.value)(ci(r)()(d.value)(u))))(Cg(t)(n)(d.value)(u))}}}}}}}}}};var rD=function(){var t=yp(jm);return function(e){return Ap(t(e))}}();var G5=typeof Array.from=="function",U5=typeof Symbol<"u"&&Symbol!=null&&typeof Symbol.iterator<"u"&&typeof String.prototype[Symbol.iterator]=="function",q5=typeof String.prototype.fromCodePoint=="function",H5=typeof String.prototype.codePointAt=="function";var mi={proof:function(t){return t},Coercible0:function(){}},aD=function(t){return t.proof};var ku=void 0;var cs=function(t){return t.toInt},Tg=function(t){return function(e){return cs(t)(ku)}};var Za={toInt:function(t){return 8}},xg={Nat0:function(){return Za}},Wo={toInt:function(t){return 7}},Fg={Nat0:function(){return Wo}},Bo={toInt:function(t){return 6}},$g={Nat0:function(){return Bo}},xa={toInt:function(t){return 5}},ls={Nat0:function(){return xa}},Un={toInt:function(t){return 4}},aa={Nat0:function(){return Un}},qn={toInt:function(t){return 3}},gu={Nat0:function(){return qn}},Hn={toInt:function(t){return 2}},Cu={Nat0:function(){return Hn}},zn={toInt:function(t){return 1}},Eu={Nat0:function(){return zn}},Mr={toInt:function(t){return 0}};var we=function(t){return function(){return function(e){return function(){return function(r){return{Nat0:e.Nat1,Pos1:function(){return t}}}}}}};var lo={Nat0:function(){return Wo},Nat1:function(){return Za}};var _o={Nat0:function(){return Bo},Nat1:function(){return Za}};var po={Nat0:function(){return xa},Nat1:function(){return Za}};var so={Nat0:function(){return Un},Nat1:function(){return Za}};var ma={Nat0:function(){return Un},Nat1:function(){return xa}};var mo={Nat0:function(){return qn},Nat1:function(){return Za}};var va={Nat0:function(){return qn},Nat1:function(){return xa}};var vo={Nat0:function(){return Hn},Nat1:function(){return Za}};var Da={Nat0:function(){return Hn},Nat1:function(){return xa}};var Do={Nat0:function(){return zn},Nat1:function(){return Za}};var da={Nat0:function(){return zn},Nat1:function(){return xa}};var bo={Nat0:function(){return Mr},Nat1:function(){return Za}};var ba={Nat0:function(){return Mr},Nat1:function(){return xa}};var Mg={Nat0:function(){return Mr},Nat1:function(){return Za}};var uD={Nat0:function(){return Mr},Nat1:function(){return Wo}};var oD={Nat0:function(){return Mr},Nat1:function(){return Bo}};var D_={Nat0:function(){return Mr},Nat1:function(){return xa}};var qa={Nat0:function(){return Mr},Nat1:function(){return Un}};var vn={Nat0:function(){return Mr},Nat1:function(){return qn}};var Dn={Nat0:function(){return Mr},Nat1:function(){return Hn}};var dn={Nat0:function(){return Mr},Nat1:function(){return zn}},Su={Nat0:function(){return Mr},Nat1:function(){return Mr}};var wg=ei;var _s=function(t){return t};var d_=function(t){return function(){return function(e){return function(r){return e[cs(t)(r)]}}}};var ps=function(t){return function(e){var r=Tg(t)(d.value),n=function(){return r===0?[]:pn(0)(r-1|0)}();return _($e)(e)(n)}};var Gu=[];var Pe=function(t){return function(e){return function(r){return Bi(e)(r)}}};var bn={first:function(t){return function(e){return new at(t(e.value0),e.value1)}},second:_(ti),Profunctor0:function(){return Bn}},Vn=function(t){return t.second},ss=function(t){return t.first};var Aw=function(t){return function(e){return function(r){return function(n){return Po(r)(t)(e)(n)}}}};var Rg=function(){return function(){return function(t){return Aw(Ln())(Ln())(t)}}};var Ng=function(){return function(){return function(t){return Rg()()(t)}}};var Cw=function(t){return function(e){return function(r){return Po(e.Profunctor0())(t)(function(n){return n.value1(n.value0)})(ss(e)(r))}}},Lg=function(t){return function(e){return function(r){return Cw(function(n){return new at(t(n),function(a){return e(n)(a)})})(r)}}};var Wg=function(t){return function(){return function(){return function(e){return function(r){return Lg(ci(t)()(e))(xt(QA(t)()()(e)))(r)}}}}};var Bg=function(t){return t};var xw=JSON.parse;var Fw=JSON.stringify;var ms=function(t){return t};var vs=function(t){return t};var Ds=function(t){return function(e){return t(e)}},b_=function(t){return{map:function(e){return Ds(_(t)(_(yf)(e)))}}};var cD=function(t){return{Applicative0:function(){return y_(t)},Bind1:function(){return lD(t)}}},lD=function(t){return{bind:function(e){return function(r){return q(t.Bind1())(e)(Ra(function(){var n=T(t.Applicative0());return function(a){return n(jt.create(a))}}())(function(n){var a=r(n);return a}))}},Apply0:function(){return Ug(t)}}},Ug=function(t){return{apply:ju(cD(t)),Functor0:function(){return b_(t.Bind1().Apply0().Functor0())}}},y_=function(t){return{pure:function(){var e=T(t.Applicative0());return function(r){return ms(e(Xt.create(r)))}}(),Apply0:function(){return Ug(t)}}};var qg=function(t){return{throwError:function(){var e=T(t.Applicative0());return function(r){return ms(e(jt.create(r)))}}(),Monad0:function(){return cD(t)}}};var _D=function(t){return function(e){return{alt:function(r){return function(n){return q(e.Bind1())(r)(function(a){if(a instanceof Xt)return T(e.Applicative0())(new Xt(a.value0));if(a instanceof jt)return q(e.Bind1())(n)(function(u){if(u instanceof Xt)return T(e.Applicative0())(new Xt(u.value0));if(u instanceof jt)return T(e.Applicative0())(new jt(bt(t)(a.value0)(u.value0)));throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): "+[u.constructor.name])});throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): "+[a.constructor.name])})}},Functor0:function(){return b_(e.Bind1().Apply0().Functor0())}}}};var pD=function(){var t=Cr();return function(e){return t(vs(e))}}();function zg(t,e,r){return t==null?e:r(t)}var Zr=function(t){return zg(t,V.value,B.create)};function A_(t){return Object.prototype.toString.call(t).slice(8,-1)}var Yg=Array.isArray||function(t){return Object.prototype.toString.call(t)==="[object Array]"};var dD=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}();var Zg=lt;var bD=function(t){var e=fo(qg(t));return function(r){return e(rD(r))}};var yD=function(t){return function(e){return function(r){if(A_(r)===e)return T(y_(t))(Zg(r));if(nr)return bD(t)(new dD(e,A_(r)));throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): "+[e.constructor.name,r.constructor.name])}}};var AD=function(t){return yD(t)("String")};var ys=function(){function t(){}return t.value=new t,t}(),As=function(){function t(){}return t.value=new t,t}(),rC=function(){function t(){}return t.value=new t,t}(),nC=function(){function t(){}return t.value=new t,t}(),gD=function(){function t(){}return t.value=new t,t}(),aC=function(){function t(){}return t.value=new t,t}(),uC=function(){function t(){}return t.value=new t,t}();var oC=function(t){return t},iC=function(t){return t};var fC=function(t){return t};var cC=function(t){return t};var lC=function(t){return t};var _C=function(t){return t},pC=function(t){return t},sC=function(t){return t},mC=function(t){return t},vC=function(t){return t};var CD=function(){function t(){}return t.value=new t,t}(),DC=function(){function t(){}return t.value=new t,t}(),dC=function(){function t(){}return t.value=new t,t}(),ED=function(){function t(){}return t.value=new t,t}(),bC=function(){function t(){}return t.value=new t,t}();var ks=function(t){return t};var Fc=function(t){return t};var pP=function(t){return t},k_=function(t){return t};var Rf={toAudioOnOff:tt(et)};var Nf=function(t){return t.toAudioParameter},yC=function(t){return t.toAudioOnOff},AC=function(){return sc.create}(),kC=function(){return mc.value}();var gs=function(){return Bg(function(){var t=Ng()()(Bn),e=Wg({reflectSymbol:function(){return"o"}})()()(d.value)(bn);return function(r){return t(e(r))}}())},gC=lt;var sP=function(){var t=ir()({reflectSymbol:function(){return"unit"}})(d.value);return function(e){return k_(t(e))}}();var mP=function(t){return function(e){return{toAudioParameter:function(r){return sP(r)}}}},CC=function(t){return function(e){return{toAudioParameter:function(){var r=Nf(mP(t)(e));return function(n){return r(pP(function(a){return{u:a}}(n)))}}()}}},EC=function(){return ir()({reflectSymbol:function(){return"2x"}})(d.value)(void 0)}(),SC=function(){var t=ir()({reflectSymbol:function(){return"sudden"}})(d.value);return function(e){return k_(t(e))}}();var hC={toAudioParameter:SC},Cs={toAudioParameter:function(t){return SC({n:t})}},SD=function(){return ir()({reflectSymbol:function(){return"step"}})(d.value)(void 0)}();var hD=function(){return ir()({reflectSymbol:function(){return"on"}})(d.value)(void 0)}(),g_={x:hD,o:0},_t=function(){return Y(h(k))(nn()(ir()({reflectSymbol:function(){return"onOff"}})(d.value)(g_)))};var TC=function(){return ir()({reflectSymbol:function(){return"off"}})(d.value)(void 0)}();var vP=function(){var t=ir()({reflectSymbol:function(){return"numeric"}})(d.value);return function(e){return k_(t(e))}}();var Wr={toAudioParameter:vP};var Go=function(){return ir()({reflectSymbol:function(){return"linear"}})(d.value)(void 0)}();var xC=function(){return ir()({reflectSymbol:function(){return"exponential"}})(d.value)(void 0)}(),DP=function(){var t=ir()({reflectSymbol:function(){return"envelope"}})(d.value);return function(e){return k_(t(e))}}();var wn={toAudioParameter:DP},dP=function(){var t=ir()({reflectSymbol:function(){return"cancel"}})(d.value);return function(e){return k_(t(e))}}();var FC={toAudioParameter:dP};var bP=function(){function t(){}return t.value=new t,t}(),yP=function(){function t(){}return t.value=new t,t}(),AP=function(){function t(){}return t.value=new t,t}(),kP=function(){function t(){}return t.value=new t,t}(),gP=function(){function t(){}return t.value=new t,t}(),CP=function(){function t(){}return t.value=new t,t}(),EP=function(){function t(){}return t.value=new t,t}(),SP=function(){function t(){}return t.value=new t,t}(),hP=function(){function t(){}return t.value=new t,t}(),TP=function(){function t(){}return t.value=new t,t}(),xP=function(){function t(){}return t.value=new t,t}(),FP=function(){function t(){}return t.value=new t,t}(),$P=function(){function t(){}return t.value=new t,t}(),MP=function(){function t(){}return t.value=new t,t}(),vi=function(t){return{toPeriodicOscSpec:function(e){return ir()({reflectSymbol:function(){return"realImg"}})(d.value)({real:_s(e.value0),img:_s(e.value1)})}}};var Es={toInitializeTriangleOsc:function(t){return vC(function(e){return{frequency:e}}(t))}};var $C={toInitializeStereoPanner:function(t){return mC(function(e){return{pan:e}}(t))}};var $c={toInitializeSquareOsc:function(t){return sC(function(e){return{frequency:e}}(t))}};var rf={toInitializeSinOsc:function(t){return pC(function(e){return{frequency:e}}(t))}};var MC={toInitializeSawtoothOsc:function(t){return _C(function(e){return{frequency:e}}(t))}};var TD={toInitializeRecorder:function(t){return oC(function(e){return{cb:e}}(t))}};var C_={toInitializeMicrophone:function(t){return iC(function(e){return{microphone:e}}(t))}};var wC=function(t){return function(e){return{toInitializeIIRFilter:function(r){return function(n){return function(a){return{feedforward:aD(mi)(Ln()(r.value0)),feedback:aD(mi)(Ln()(r.value1))}}}}}}};var ft={toInitializeGain:function(t){return lC(function(e){return{gain:e}}(t))}};var PC={toInitializeConvolver:function(t){return fC(function(e){return{buffer:e}}(t))}},Ss={toInitializeConstant:function(t){return cC(function(e){return{offset:e}}(t))}};var wP={convertOption:function(t){return function(e){return tt(et)}}},E_={convertOption:function(t){return function(e){return tt(et)}}},OC={convertOption:function(t){return function(e){return tt(et)}}},IC={convertOption:function(t){return function(e){return B.create}}},RC={convertOption:function(t){return function(e){return tt(et)}}},Di={convertOption:function(t){return function(e){return tt(et)}}},Mc={convertOption:function(t){return function(e){return tt(et)}}},wc={convertOption:function(t){return function(e){return tt(et)}}},Pc={convertOption:function(t){return function(e){return tt(et)}}},Oc={convertOption:function(t){return function(e){return tt(et)}}},Ic={convertOption:function(t){return function(e){return tt(et)}}},NC={convertOption:function(t){return function(e){return tt(et)}}},LC={convertOption:function(t){return function(e){return tt(et)}}},WC={convertOption:function(t){return function(e){return tt(et)}}},xD={convertOption:function(t){return function(e){return tt(et)}}},Lf={convertOption:function(t){return function(e){return tt(et)}}},S_={convertOption:function(t){return function(e){return tt(et)}}},h_={convertOption:function(t){return function(e){return tt(et)}}};var Rc={convertOption:function(t){return function(e){return tt(et)}}},BC={convertOption:function(t){return function(e){return tt(et)}}},GC={convertOption:function(t){return function(e){return tt(et)}}},UC={convertOption:function(t){return function(e){return tt(et)}}},FD={convertOption:function(t){return function(e){return tt(et)}}};var qC={convertOption:function(t){return function(e){return tt(et)}}},$D={convertOption:function(t){return function(e){return tt(et)}}},An={convertOption:function(t){return function(e){return tt(et)}}},fn={convertOption:function(t){return function(e){return tt(et)}}},MD={convertOption:function(t){return function(e){return tt(et)}}},hs={convertOption:function(t){return function(e){return tt(et)}}},PP=function(t){return t.toPeriodicOscSpec},di=function(t){return{convertOption:function(e){return function(r){return PP(t)}}}},wD=function(t){return t.toInitializeWaveShaper},HC=function(t){return t.toInitializeTriangleOsc},zC=function(t){return t.toInitializeStereoPanner},VC=function(t){return t.toInitializeSquareOsc},JC=function(t){return t.toInitializeSinOsc},jC=function(t){return t.toInitializeSawtoothOsc},XC=function(t){return t.toInitializeRecorder},PD=function(t){return t.toInitializePlayBuf},QC=function(t){return t.toInitializePeriodicOsc},KC=function(t){return t.toInitializePeaking},YC=function(t){return t.toInitializeNotch},ZC=function(t){return t.toInitializeMicrophone},tE=function(t){return t.toInitializeLowshelf},OD=function(t){return t.toInitializeLowpass},ID=function(t){return t.toInitializeLoopBuf},eE=function(t){return t.toInitializeIIRFilter},rE=function(t){return t.toInitializeHighshelf},RD=function(t){return t.toInitializeHighpass},nE=function(t){return t.toInitializeGain},aE=function(t){return t.toInitializeDynamicsCompressor},ND=function(t){return t.toInitializeDelay},uE=function(t){return t.toInitializeConvolver},oE=function(t){return t.toInitializeConstant},LD=function(t){return t.toInitializeBandpass},WD=function(t){return t.toInitializeAllpass};var OP={oversample:EC},IP=function(t){return{toInitializeWaveShaper:function(e){return sa(t)(bP.value)(OP)(e)}}},iE={toInitializeWaveShaper:function(){var t=wD(IP(Ct(gt()(J(kt)(wP)()()()({reflectSymbol:function(){return"curve"}})))(At()())));return function(e){return t(function(r){return{curve:r}}(e))}}()},RP=function(){return{bufferOffset:0,playbackRate:1,duration:V.value}}(),T_=function(t){return{toInitializePlayBuf:function(e){return sa(t)(yP.value)(RP)(e)}}},Ha={toInitializePlayBuf:function(){var t=PD(T_(Ct(gt()(J(kt)(E_)()()()({reflectSymbol:function(){return"buffer"}})))(At()())));return function(e){return t(function(r){return{buffer:r}}(e))}}()},NP={},bi=function(t){return{toInitializePeriodicOsc:function(e){return sa(t)(AP.value)(NP)(e)}}},LP={q:1,gain:0},Nc=function(t){return{toInitializePeaking:function(e){return sa(t)(kP.value)(LP)(e)}}};var WP={q:1},Lc=function(t){return{toInitializeNotch:function(e){return sa(t)(gP.value)(WP)(e)}}};var BP={gain:0},fE=function(t){return{toInitializeLowshelf:function(e){return sa(t)(CP.value)(BP)(e)}}};var GP={q:1},BD=function(t){return{toInitializeLowpass:function(e){return sa(t)(EP.value)(GP)(e)}}},Ts={toInitializeLowpass:function(){var t=OD(BD(Ct(gt()(J(kt)(xD)()()()({reflectSymbol:function(){return"frequency"}})))(At()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()},UP=function(){return{loopStart:0,loopEnd:0,playbackRate:1,duration:V.value}}(),Wf=function(t){return{toInitializeLoopBuf:function(e){return sa(t)(SP.value)(UP)(e)}}},Ce={toInitializeLoopBuf:function(){var t=ID(Wf(Ct(gt()(J(kt)(Lf)()()()({reflectSymbol:function(){return"buffer"}})))(At()())));return function(e){return t(function(r){return{buffer:r}}(e))}}()},qP={gain:0},cE=function(t){return{toInitializeHighshelf:function(e){return sa(t)(hP.value)(qP)(e)}}};var HP={q:1},GD=function(t){return{toInitializeHighpass:function(e){return sa(t)(TP.value)(HP)(e)}}},tu={toInitializeHighpass:function(){var t=RD(GD(Ct(gt()(J(kt)(FD)()()()({reflectSymbol:function(){return"frequency"}})))(At()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()},zP=function(){return{ratio:12,attack:.003,release:.25,knee:30,threshold:-24}}(),lE=function(t){return{toInitializeDynamicsCompressor:function(e){return sa(t)(xP.value)(zP)(e)}}},VP={maxDelayTime:1},UD=function(t){return{toInitializeDelay:function(e){return sa(t)(FP.value)(VP)(e)}}},tn={toInitializeDelay:function(){var t=ND(UD(Ct(gt()(J(kt)($D)()()()({reflectSymbol:function(){return"delayTime"}})))(At()())));return function(e){return t(function(r){return{delayTime:r}}(e))}}()},JP={q:1},cn=function(t){return{toInitializeBandpass:function(e){return sa(t)($P.value)(JP)(e)}}},qD={toInitializeBandpass:function(){var t=LD(cn(Ct(gt()(J(kt)(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()},jP={q:1},xs=function(t){return{toInitializeAllpass:function(e){return sa(t)(MP.value)(jP)(e)}}},HD={toInitializeAllpass:function(){var t=WD(xs(Ct(gt()(J(kt)(hs)()()()({reflectSymbol:function(){return"frequency"}})))(At()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()};var Uo=function(){function t(){this.head=null,this.last=null,this.size=0}function e(l,v){this.queue=l,this.value=v,this.next=null,this.prev=null}function r(l){this.draining=!1,this.error=null,this.value=l,this.takes=new t,this.reads=new t,this.puts=new t}var n={};function a(l){try{l()}catch(v){setTimeout(function(){throw v},0)}}function u(l,v){var D=new e(l,v);switch(l.size){case 0:l.head=D;break;case 1:D.prev=l.head,l.head.next=D,l.last=D;break;default:D.prev=l.last,l.last.next=D,l.last=D}return l.size++,D}function i(l){var v;switch(l.size){case 0:return null;case 1:v=l.head,l.head=null;break;case 2:v=l.last,l.head.next=null,l.last=null;break;default:v=l.last,l.last=v.prev,l.last.next=null}return v.prev=null,v.queue=null,l.size--,v.value}function o(l){var v;switch(l.size){case 0:return null;case 1:v=l.head,l.head=null;break;case 2:v=l.head,l.last.prev=null,l.head=l.last,l.last=null;break;default:v=l.head,l.head=v.next,l.head.prev=null}return v.next=null,v.queue=null,l.size--,v.value}function m(l){if(l.queue!==null){if(l.queue.last===l){i(l.queue);return}if(l.queue.head===l){o(l.queue);return}l.prev&&(l.prev.next=l.next),l.next&&(l.next.prev=l.prev),l.queue.size--,l.queue=null,l.value=null,l.next=null,l.prev=null}}function s(l,v){if(!v.draining){var D=v.puts,c=v.takes,C=v.reads,ut,mt,ve,ne,Ze;for(v.draining=!0;;){if(ut=null,mt=null,ve=null,ne=v.value,Ze=C.size,v.error!==null){for(ne=l.left(v.error);ut=o(D);)a(ut.cb(ne));for(;mt=o(C);)a(mt(ne));for(;ve=o(c);)a(ve(ne));break}if(ne===n&&(ut=o(D))&&(v.value=ne=ut.value),ne!==n){for(ve=o(c);Ze--&&(mt=o(C));)a(mt(l.right(ne)));ve!==null&&(v.value=n,a(ve(l.right(ne))))}if(ut!==null&&a(ut.cb(l.right(void 0))),v.value===n&&D.size===0||v.value!==n&&c.size===0)break}v.draining=!1}}return r.EMPTY=n,r.putLast=u,r.takeLast=i,r.takeHead=o,r.deleteCell=m,r.drainVar=s,r}();function x_(){return new Uo(Uo.EMPTY)}function _E(t,e,r){return function(){var n=Uo.putLast(e.takes,r);return Uo.drainVar(t,e),function(){Uo.deleteCell(n)}}}function pE(t,e,r){return function(){return r.value===Uo.EMPTY&&r.error===null?(r.value=e,Uo.drainVar(t,r),!0):!1}}function sE(t,e){return function(){var r=e.value;return r===Uo.EMPTY?t.nothing:(e.value=Uo.EMPTY,Uo.drainVar(t,e),t.just(r))}}var YP=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),ZP=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),tO=function(){function t(){}return t.value=new t,t}();var zD=function(){return{left:jt.create,right:Xt.create,nothing:V.value,just:B.create,killed:YP.create,filled:ZP.create,empty:tO.value}}();var mE=function(t){return function(e){return _E(zD,t,e)}},Fs=function(t){return function(e){return pE(zD,t,e)}};var vE=function(t){return sE(zD,t)};var eO=function(t,e,r){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return n=1,a=r(),n=2,a}};var rO=function(){function t(){}return t.value=new t,t}();var $s={convertOption:function(t){return function(e){return tt(et)}}},Ms={convertOption:function(t){return function(e){return tt(et)}}};var nO=function(t){return t.toInitializeAnalyser},eu=mk(k)({doLogic:Df,ids:function(){var t=Cr();return function(e){return function(r){return r.ids}(t(e))}}(),disconnectElement:function(t){return function(e){return t.disconnectXFromY({from:e.id,to:e.parent})}},toElt:function(t){return t}})({fromElt:Ln(),connectToParent:function(t){return function(e){return t.connectXToY({from:e.id,to:e.parent})}}});var aO=function(){return{cb:function(t){return T(f)(T(f)(void 0))},fftSize:gD.value,maxDecibels:-30,minDecibels:-100,smoothingTimeConstant:.8,channelCount:2,channelCountMode:ED.value,channelInterpretation:CD.value}}(),ws=function(t){return{toInitializeAnalyser:function(e){return sa(t)(rO.value)(aO)(e)}}};var uO=function(t){return function(e){var r=ZC(t)(e),n=function(a){return function(u){return Wt(function(i){return function(){var m=u.ids();return a.raiseId(m)(),_(F)(function(s){return X(nt)(i(u.deleteFromCache({id:m})))(s)})(xt($t)(i)(Mt(f)(u.makeMicrophone({id:m,parent:a.parent,scope:a.scope,microphone:r.microphone}))))()}})}};return new O(n)}},F_=function(t){return uO(t)};var Jn=oo(f)(k)({doLogic:Df,ids:function(){var t=Cr();return function(e){return function(r){return r.ids}(t(e))}}(),disconnectElement:function(t){return function(e){return t.disconnectXFromY({from:e.id,to:e.parent})}},toElt:function(t){return t}}),oO=function(t){return function(e){return function(r){return function(n){var a=nO(t)(e),u=function(i){return function(o){return Wt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt($t)(m)(N(I(f))(Mt(f)(o.makeAnalyser({id:l,parent:i.parent,scope:i.scope,cb:a.cb,fftSize:Ov(2)(function(){if(a.fftSize instanceof ys)return 7;if(a.fftSize instanceof As)return 8;if(a.fftSize instanceof rC)return 9;if(a.fftSize instanceof nC)return 10;if(a.fftSize instanceof gD)return 11;if(a.fftSize instanceof aC)return 12;if(a.fftSize instanceof uC)return 13;throw new Error("Failed pattern match at WAGS.Control (line 189, column 21 - line 196, column 34): "+[a.fftSize.constructor.name])}()),maxDecibels:a.maxDecibels,minDecibels:a.minDecibels,smoothingTimeConstant:a.smoothingTimeConstant,channelCount:a.channelCount,channelCountMode:function(){if(a.channelCountMode instanceof bC)return"explicit";if(a.channelCountMode instanceof ED)return"max";if(a.channelCountMode instanceof dC)return"clamped-max";throw new Error("Failed pattern match at WAGS.Control (line 202, column 35 - line 205, column 46): "+[a.channelCountMode.constructor.name])}(),channelInterpretation:function(){if(a.channelInterpretation instanceof CD)return"speakers";if(a.channelInterpretation instanceof DC)return"discrete";throw new Error("Failed pattern match at WAGS.Control (line 206, column 40 - line 208, column 41): "+[a.channelInterpretation.constructor.name])}()})))(N(I(f))(_(g)(function(v){return Ur()()()({cb:function(D){return o.setAnalyserNodeCb({id:l,cb:D})}})(v)})(r))(Jn({parent:new B(l),scope:i.scope,raiseId:Pt(Qr(gr(Ir)))})(o)(G(n))))))()}})}};return new O(u)}}}},Ps=function(t){return function(e){return oO(t)(e)(w(S(f)))}},dE=function(t){return function(e){return function(r){var n=uE(t)(e),a=function(u){return function(i){return Wt(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(F)(function(l){return X(nt)(o(i.deleteFromCache({id:s})))(l)})(xt($t)(o)(N(I(f))(Mt(f)(i.makeConvolver({id:s,parent:u.parent,scope:u.scope,buffer:n.buffer})))(Jn({parent:new B(s),scope:u.scope,raiseId:Pt(Qr(gr(Ir)))})(i)(G(r)))))()}})}};return new O(a)}}},iO=function(){return function(){return function(t){return function(e){return function(r){return function(n){return function(a){var u=eE(t)(n)(e)(r),i=function(o){return function(m){return Wt(function(s){return function(){var v=m.ids();return o.raiseId(v)(),_(F)(function(D){return X(nt)(s(m.deleteFromCache({id:v})))(D)})(xt($t)(s)(N(I(f))(Mt(f)(m.makeIIRFilter({id:v,parent:o.parent,scope:o.scope,feedforward:hf()(u.feedforward),feedback:hf()(u.feedback)})))(Jn({parent:new B(v),scope:o.scope,raiseId:Pt(Qr(gr(Ir)))})(m)(G(a)))))()}})}};return new O(i)}}}}}}},bE=function(){return function(){return function(t){return iO()()(t)(d.value)(d.value)}}},VD=function(t){return function(e){return function(r){var n=XC(t)(e),a=function(u){return function(i){return Wt(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(F)(function(l){return X(nt)(o(i.deleteFromCache({id:s})))(l)})(xt($t)(o)(N(I(f))(Mt(f)(i.makeRecorder({id:s,parent:u.parent,scope:u.scope,cb:n.cb})))(Jn({parent:new B(s),scope:u.scope,raiseId:Pt(Qr(gr(Ir)))})(i)(r))))()}})}};return new O(a)}}},fO=function(t){return function(e){return Wt(function(r){return function(){var a=e.ids();return r(e.makeSpeaker({id:a}))(),$t(Jn({parent:new B(a),scope:new Ki("toplevel"),raiseId:Pt(Qr(gr(Ir)))})(e)(G(t)))(r)()}})}},Bf=fO,Ot=function(t){return function(e){return function(r){return qr(t)(e)(w(S(f)))(r)}}},qr=function(t){return function(e){return function(r){return function(n){var a=nE(t)(e),u=function(i){return function(o){return Wt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt($t)(m)(N(I(f))(Mt(f)(o.makeGain({id:l,parent:i.parent,scope:i.scope,gain:a.gain})))(N(I(f))($r(h(k))(_(g)(function(v){return Ur()()()({gain:yE(591)(i.scope)(o)(function(D){return o.setGain(function(c){return{id:l,gain:c}}(D))})})(v)})(r)))(Jn({parent:new B(l),scope:i.scope,raiseId:Pt(Qr(gr(Ir)))})(o)(G(n))))))()}})}};return new O(u)}}}},yE=eO("tmpResolveAU","WAGS.Control",function(){var t=function(){var i=ir()({reflectSymbol:function(){return"unit"}})(d.value);return function(o){return Fc(i(o))}}(),e=function(){var i=ir()({reflectSymbol:function(){return"sudden"}})(d.value);return function(o){return Fc(i(o))}}(),r=function(){var i=ir()({reflectSymbol:function(){return"numeric"}})(d.value);return function(o){return Fc(i(o))}}(),n=function(){var i=ir()({reflectSymbol:function(){return"envelope"}})(d.value);return function(o){return Fc(i(o))}}(),a=function(){var i=ir()({reflectSymbol:function(){return"cancel"}})(d.value);return function(o){return Fc(i(o))}}(),u=function(i){return function(o){return function(m){return function(s){return Ur()()()({numeric:function(){var l=Mt(f);return function(v){return l(m(r(v)))}}(),envelope:function(){var l=Mt(f);return function(v){return l(m(n(v)))}}(),cancel:function(){var l=Mt(f);return function(v){return l(m(a(v)))}}(),sudden:function(){var l=Mt(f);return function(v){return l(m(e(v)))}}(),unit:function(l){var v=Ot(ft)(1)([l.u]);return Wt(function(D){return function(){var C=x_();return $t(N(I(f))(Jn({parent:V.value,scope:i,raiseId:function(ut){return Ae(F)(Fs(ut)(C))}})(o)(v))(Wt(function(ut){return function(){return Ae(F)(mE(C)(function(ve){if(ve instanceof jt)return $f(ve.value0);if(ve instanceof Xt)return ut(m(t({i:ve.value0})));throw new Error("Failed pattern match at WAGS.Control (line 1674, column 39 - line 1677, column 66): "+[ve.constructor.name])}))(),T(f)(void 0)}})))(D)()}})}})(s)}}}};return u}),cr=yE(1653),cO=function(t){return function(e){return function(r){var n=ID(t)(e),a=function(u){return function(i){return Wt(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(F)(function(l){return X(nt)(o(i.deleteFromCache({id:s})))(l)})(xt($t)(o)(N(I(f))(Mt(f)(i.makeLoopBuf({id:s,parent:u.parent,scope:u.scope,buffer:n.buffer,playbackRate:n.playbackRate,loopStart:n.loopStart,loopEnd:n.loopEnd,duration:n.duration})))($r(h(k))(_(g)(function(l){return Ur()()()({buffer:function(v){return Mt(f)(i.setBuffer({id:s,buffer:v}))},playbackRate:cr(u.scope)(i)(function(v){return i.setPlaybackRate(function(D){return{id:s,playbackRate:D}}(v))}),loopStart:function(v){return Mt(f)(i.setLoopStart({id:s,loopStart:v}))},loopEnd:function(v){return Mt(f)(i.setLoopEnd({id:s,loopEnd:v}))},onOff:function(v){return Mt(f)(i.setOnOff({id:s,onOff:v}))}})(l)})(r)))))()}})}};return new O(a)}}},le=function(t){return cO(t)};var lO=function(t){return function(e){return function(r){var n=QC(t)(e),a=function(u){return function(i){return Wt(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(F)(function(l){return X(nt)(o(i.deleteFromCache({id:s})))(l)})(xt($t)(o)(N(I(f))(Mt(f)(i.makePeriodicOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency,spec:n.spec})))($r(h(k))(_(g)(function(l){return Ur()()()({frequency:cr(u.scope)(i)(function(v){return i.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return Mt(f)(i.setOnOff({id:s,onOff:v}))},spec:function(v){return Mt(f)(i.setPeriodicOsc({id:s,spec:v}))}})(l)})(r)))))()}})}};return new O(a)}}},yi=function(t){return lO(t)};var _O=function(t){return function(e){return function(r){var n=PD(t)(e),a=function(u){return function(i){return Wt(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(F)(function(l){return X(nt)(o(i.deleteFromCache({id:s})))(l)})(xt($t)(o)(N(I(f))(Mt(f)(i.makePlayBuf({id:s,parent:u.parent,scope:u.scope,buffer:n.buffer,playbackRate:n.playbackRate,bufferOffset:n.bufferOffset,duration:n.duration})))($r(h(k))(_(g)(function(l){return Ur()()()({buffer:function(v){return Mt(f)(i.setBuffer({id:s,buffer:v}))},playbackRate:cr(u.scope)(i)(function(v){return i.setPlaybackRate(function(D){return{id:s,playbackRate:D}}(v))}),bufferOffset:function(v){return Mt(f)(i.setBufferOffset({id:s,bufferOffset:v}))},onOff:function(v){return Mt(f)(i.setOnOff({id:s,onOff:v}))},duration:function(v){return Mt(f)(i.setDuration({id:s,duration:v}))}})(l)})(r)))))()}})}};return new O(a)}}},jn=function(t){return _O(t)};var pO=function(t){return function(e){return function(r){var n=jC(t)(e),a=function(u){return function(i){return Wt(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(F)(function(l){return X(nt)(o(i.deleteFromCache({id:s})))(l)})(xt($t)(o)(N(I(f))(Mt(f)(i.makeSawtoothOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))($r(h(k))(_(g)(function(l){return Ur()()()({frequency:cr(u.scope)(i)(function(v){return i.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return Mt(f)(i.setOnOff({id:s,onOff:v}))}})(l)})(r)))))()}})}};return new O(a)}}},AE=function(t){return pO(t)};var sO=function(t){return function(e){return function(r){var n=JC(t)(e),a=function(u){return function(i){return Wt(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(F)(function(l){return X(nt)(o(i.deleteFromCache({id:s})))(l)})(xt($t)(o)(N(I(f))(Mt(f)(i.makeSinOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))($r(h(k))(_(g)(function(l){return Ur()()()({frequency:cr(u.scope)(i)(function(v){return i.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return Mt(f)(i.setOnOff({id:s,onOff:v}))}})(l)})(r)))))()}})}};return new O(a)}}},Gf=function(t){return sO(t)},kE=function(t){return function(e){return Gf(t)(e)(w(S(f)))}},mO=function(t){return function(e){return function(r){var n=VC(t)(e),a=function(u){return function(i){return Wt(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(F)(function(l){return X(nt)(o(i.deleteFromCache({id:s})))(l)})(xt($t)(o)(N(I(f))(Mt(f)(i.makeSquareOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))($r(h(k))(_(g)(function(l){return Ur()()()({frequency:cr(u.scope)(i)(function(v){return i.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return Mt(f)(i.setOnOff({id:s,onOff:v}))}})(l)})(r)))))()}})}};return new O(a)}}},$_=function(t){return mO(t)},gE=function(t){return function(e){return $_(t)(e)(w(S(f)))}},vO=function(t){return function(e){return function(r){var n=HC(t)(e),a=function(u){return function(i){return Wt(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(F)(function(l){return X(nt)(o(i.deleteFromCache({id:s})))(l)})(xt($t)(o)(N(I(f))(Mt(f)(i.makeTriangleOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))($r(h(k))(_(g)(function(l){return Ur()()()({frequency:cr(u.scope)(i)(function(v){return i.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return Mt(f)(i.setOnOff({id:s,onOff:v}))}})(l)})(r)))))()}})}};return new O(a)}}},Os=function(t){return vO(t)};var DO=function(t){return function(e){return function(r){return function(n){var a=WD(t)(e),u=function(i){return function(o){return Wt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt($t)(m)(N(I(f))(Mt(f)(o.makeAllpass({id:l,parent:i.parent,scope:i.scope,frequency:a.frequency,q:a.q})))(N(I(f))($r(h(k))(_(g)(function(v){return Ur()()()({frequency:cr(i.scope)(o)(function(D){return o.setFrequency(function(c){return{id:l,frequency:c}}(D))}),q:cr(i.scope)(o)(function(D){return o.setQ(function(c){return{id:l,q:c}}(D))})})(v)})(r)))(Jn({parent:new B(l),scope:i.scope,raiseId:Pt(Qr(gr(Ir)))})(o)(G(n))))))()}})}};return new O(u)}}}},M_=function(t){return function(e){return function(r){return DO(t)(e)(w(S(f)))(r)}}},JD=function(t){return function(e){return function(r){return function(n){var a=LD(t)(e),u=function(i){return function(o){return Wt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt($t)(m)(N(I(f))(Mt(f)(o.makeBandpass({id:l,parent:i.parent,scope:i.scope,frequency:a.frequency,q:a.q})))(N(I(f))($r(h(k))(_(g)(function(v){return Ur()()()({frequency:cr(i.scope)(o)(function(D){return o.setFrequency(function(c){return{id:l,frequency:c}}(D))}),q:cr(i.scope)(o)(function(D){return o.setQ(function(c){return{id:l,q:c}}(D))})})(v)})(r)))(Jn({parent:new B(l),scope:i.scope,raiseId:Pt(Qr(gr(Ir)))})(o)(G(n))))))()}})}};return new O(u)}}}},kn=function(t){return function(e){return function(r){return JD(t)(e)(w(S(f)))(r)}}},w_=function(t){return function(e){return function(r){return function(n){var a=ND(t)(e),u=function(i){return function(o){return Wt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt($t)(m)(N(I(f))(Mt(f)(o.makeDelay({id:l,parent:i.parent,scope:i.scope,delayTime:a.delayTime,maxDelayTime:a.maxDelayTime})))(N(I(f))($r(h(k))(_(g)(function(v){return Ur()()()({delayTime:cr(i.scope)(o)(function(D){return o.setDelay(function(c){return{id:l,delayTime:c}}(D))})})(v)})(r)))(Jn({parent:new B(l),scope:i.scope,raiseId:Pt(Qr(gr(Ir)))})(o)(G(n))))))()}})}};return new O(u)}}}},yo=function(t){return function(e){return function(r){return w_(t)(e)(w(S(f)))(r)}}},dO=function(t){return function(e){return function(r){return function(n){var a=aE(t)(e),u=function(i){return function(o){return Wt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt($t)(m)(N(I(f))(Mt(f)(o.makeDynamicsCompressor({id:l,parent:i.parent,scope:i.scope,threshold:a.threshold,ratio:a.ratio,knee:a.knee,attack:a.attack,release:a.release})))(N(I(f))($r(h(k))(_(g)(function(v){return Ur()()()({threshold:cr(i.scope)(o)(function(D){return o.setThreshold(function(c){return{id:l,threshold:c}}(D))}),ratio:cr(i.scope)(o)(function(D){return o.setRatio(function(c){return{id:l,ratio:c}}(D))}),knee:cr(i.scope)(o)(function(D){return o.setKnee(function(c){return{id:l,knee:c}}(D))}),attack:cr(i.scope)(o)(function(D){return o.setAttack(function(c){return{id:l,attack:c}}(D))}),release:cr(i.scope)(o)(function(D){return o.setRelease(function(c){return{id:l,release:c}}(D))})})(v)})(r)))(Jn({parent:new B(l),scope:i.scope,raiseId:Pt(Qr(gr(Ir)))})(o)(G(n))))))()}})}};return new O(u)}}}},CE=function(t){return function(e){return dO(t)(e)(w(S(f)))}},bO=function(){return function(t){return function(e){return Sv()(k)({doLogic:Df,ids:function(){var r=Cr();return function(n){return function(a){return a.ids}(r(n))}}(),disconnectElement:function(r){return function(n){return r.disconnectXFromY({from:n.id,to:n.parent})}},toElt:function(r){return r}})({fromEltO1:Ln(),fromEltO2:Ln(),toElt:Ln(),wrapElt:function(r){return Ot(ft)(1)([r])},giveNewParent:function(r){return function(n){return function(a){return r.connectXToY({from:n.id,to:n.parent})}}},deleteFromCache:function(){var r=Cr();return function(n){return function(a){return a.deleteFromCache}(r(n))}}()})(t)(Ka(Bn)(_(jl)(function(r){return r(void 0)}))(Ln()(e)))}}},Fa=function(t){return function(e){return bO()(rk(t))(Ka(Bn)(nk()()()()()({reflectType:function(){return 0}})(d.value))(e))}};var jD=function(t){return function(e){return function(r){return function(n){var a=RD(t)(e),u=function(i){return function(o){return Wt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt($t)(m)(N(I(f))(Mt(f)(o.makeHighpass({id:l,parent:i.parent,scope:i.scope,frequency:a.frequency,q:a.q})))(N(I(f))($r(h(k))(_(g)(function(v){return Ur()()()({frequency:cr(i.scope)(o)(function(D){return o.setFrequency(function(c){return{id:l,frequency:c}}(D))}),q:cr(i.scope)(o)(function(D){return o.setQ(function(c){return{id:l,q:c}}(D))})})(v)})(r)))(Jn({parent:new B(l),scope:i.scope,raiseId:Pt(Qr(gr(Ir)))})(o)(G(n))))))()}})}};return new O(u)}}}},Wc=function(t){return function(e){return function(r){return jD(t)(e)(w(S(f)))(r)}}},yO=function(t){return function(e){return function(r){return function(n){var a=rE(t)(e),u=function(i){return function(o){return Wt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt($t)(m)(N(I(f))(Mt(f)(o.makeHighshelf({id:l,parent:i.parent,scope:i.scope,frequency:a.frequency,gain:a.gain})))(N(I(f))($r(h(k))(_(g)(function(v){return Ur()()()({frequency:cr(i.scope)(o)(function(D){return o.setFrequency(function(c){return{id:l,frequency:c}}(D))}),gain:cr(i.scope)(o)(function(D){return o.setGain(function(c){return{id:l,gain:c}}(D))})})(v)})(r)))(Jn({parent:new B(l),scope:i.scope,raiseId:Pt(Qr(gr(Ir)))})(o)(G(n))))))()}})}};return new O(u)}}}},EE=function(t){return function(e){return function(r){return yO(t)(e)(w(S(f)))(r)}}},SE=function(t){return function(e){return function(r){return function(n){var a=OD(t)(e),u=function(i){return function(o){return Wt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt($t)(m)(N(I(f))(Mt(f)(o.makeLowpass({id:l,parent:i.parent,scope:i.scope,frequency:a.frequency,q:a.q})))(N(I(f))($r(h(k))(_(g)(function(v){return Ur()()()({frequency:cr(i.scope)(o)(function(D){return o.setFrequency(function(c){return{id:l,frequency:c}}(D))}),q:cr(i.scope)(o)(function(D){return o.setQ(function(c){return{id:l,q:c}}(D))})})(v)})(r)))(Jn({parent:new B(l),scope:i.scope,raiseId:Pt(Qr(gr(Ir)))})(o)(G(n))))))()}})}};return new O(u)}}}},Bc=function(t){return function(e){return function(r){return SE(t)(e)(w(S(f)))(r)}}},AO=function(t){return function(e){return function(r){return function(n){var a=tE(t)(e),u=function(i){return function(o){return Wt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt($t)(m)(N(I(f))(Mt(f)(o.makeLowshelf({id:l,parent:i.parent,scope:i.scope,frequency:a.frequency,gain:a.gain})))(N(I(f))($r(h(k))(_(g)(function(v){return Ur()()()({frequency:cr(i.scope)(o)(function(D){return o.setFrequency(function(c){return{id:l,frequency:c}}(D))}),gain:cr(i.scope)(o)(function(D){return o.setGain(function(c){return{id:l,gain:c}}(D))})})(v)})(r)))(Jn({parent:new B(l),scope:i.scope,raiseId:Pt(Qr(gr(Ir)))})(o)(G(n))))))()}})}};return new O(u)}}}},hE=function(t){return function(e){return function(r){return AO(t)(e)(w(S(f)))(r)}}},kO=function(t){return function(e){return function(r){return function(n){var a=YC(t)(e),u=function(i){return function(o){return Wt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt($t)(m)(N(I(f))(Mt(f)(o.makeNotch({id:l,parent:i.parent,scope:i.scope,frequency:a.frequency,q:a.q})))(N(I(f))($r(h(k))(_(g)(function(v){return Ur()()()({frequency:cr(i.scope)(o)(function(D){return o.setFrequency(function(c){return{id:l,frequency:c}}(D))}),q:cr(i.scope)(o)(function(D){return o.setQ(function(c){return{id:l,q:c}}(D))})})(v)})(r)))(Jn({parent:new B(l),scope:i.scope,raiseId:Pt(Qr(gr(Ir)))})(o)(G(n))))))()}})}};return new O(u)}}}},Gc=function(t){return function(e){return function(r){return kO(t)(e)(w(S(f)))(r)}}},gO=function(t){return function(e){return function(r){return function(n){var a=zC(t)(e),u=function(i){return function(o){return Wt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt($t)(m)(N(I(f))(Mt(f)(o.makeStereoPanner({id:l,parent:i.parent,scope:i.scope,pan:a.pan})))(N(I(f))($r(h(k))(_(g)(function(v){return Ur()()()({pan:cr(i.scope)(o)(function(D){return o.setPan(function(c){return{id:l,pan:c}}(D))})})(v)})(r)))(Jn({parent:new B(l),scope:i.scope,raiseId:Pt(Qr(gr(Ir)))})(o)(G(n))))))()}})}};return new O(u)}}}},TE=function(t){return function(e){return gO(t)(e)(w(S(f)))}},CO=function(t){return function(e){return function(r){return function(n){var a=KC(t)(e),u=function(i){return function(o){return Wt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt($t)(m)(N(I(f))(Mt(f)(o.makePeaking({id:l,parent:i.parent,scope:i.scope,frequency:a.frequency,q:a.q,gain:a.gain})))(N(I(f))($r(h(k))(_(g)(function(v){return Ur()()()({frequency:cr(i.scope)(o)(function(D){return o.setFrequency(function(c){return{id:l,frequency:c}}(D))}),q:cr(i.scope)(o)(function(D){return o.setQ(function(c){return{id:l,q:c}}(D))}),gain:cr(i.scope)(o)(function(D){return o.setGain(function(c){return{id:l,gain:c}}(D))})})(v)})(r)))(Jn({parent:new B(l),scope:i.scope,raiseId:Pt(Qr(gr(Ir)))})(o)(G(n))))))()}})}};return new O(u)}}}},Uc=function(t){return function(e){return function(r){return CO(t)(e)(w(S(f)))(r)}}},xE=function(t){return function(e){return function(r){var n=wD(t)(e),a=function(u){return function(i){return Wt(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(F)(function(l){return X(nt)(o(i.deleteFromCache({id:s})))(l)})(xt($t)(o)(N(I(f))(Mt(f)(i.makeWaveShaper({id:s,parent:u.parent,scope:u.scope,curve:n.curve,oversample:n.oversample})))(Jn({parent:new B(s),scope:u.scope,raiseId:Pt(Qr(gr(Ir)))})(i)(G(r)))))()}})}};return new O(a)}}},EO=function(t){return function(e){return function(r){var n=oE(t)(e),a=function(u){return function(i){return Wt(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(F)(function(l){return X(nt)(o(i.deleteFromCache({id:s})))(l)})(xt($t)(o)(N(I(f))(Mt(f)(i.makeConstant({id:s,parent:u.parent,scope:u.scope,offset:n.offset})))($r(h(k))(_(g)(function(l){return Ur()()()({offset:cr(u.scope)(i)(function(v){return i.setOffset(function(D){return{id:s,offset:D}}(v))}),onOff:function(v){return Mt(f)(i.setOnOff({id:s,onOff:v}))}})(l)})(r)))))()}})}};return new O(a)}}},Is=function(t){return EO(t)};function XD(){window.scrollTo(0,0)}var Ao=function(t){return t.sequential},Pn=function(t){return t.parallel};var gn=function(t){return function(e){return function(r){return new O(j(t)("button")(e)(G(r)))}}};var $a=function(){var t={},e="Pure",r="Throw",n="Catch",a="Sync",u="Async",i="Bind",o="Bracket",m="Fork",s="Sequential",l="Map",v="Apply",D="Alt",c="Cons",C="Resume",ut="Release",mt="Finalizer",ve="Finalized",ne="Forked",Ze="Fiber",lr="Thunk";function pt(Gt,Xe,zr,_r){this.tag=Gt,this._1=Xe,this._2=zr,this._3=_r}function De(Gt){var Xe=function(zr,_r,ie){return new pt(Gt,zr,_r,ie)};return Xe.tag=Gt,Xe}function he(Gt){return new pt(e,void 0)}function ae(Gt){try{Gt()}catch(Xe){setTimeout(function(){throw Xe},0)}}function Hr(Gt,Xe,zr){try{return Xe(zr())}catch(_r){return Gt(_r)}}function So(Gt,Xe,zr){try{return Xe(zr)()}catch(_r){return zr(Gt(_r))(),he}}var zu=function(){var Gt=1024,Xe=0,zr=0,_r=new Array(Gt),ie=!1;function St(){var rr;for(ie=!0;Xe!==0;)Xe--,rr=_r[zr],_r[zr]=void 0,zr=(zr+1)%Gt,rr();ie=!1}return{isDraining:function(){return ie},enqueue:function(rr){var Ie,Br;Xe===Gt&&(Br=ie,St(),ie=Br),_r[(zr+Xe)%Gt]=rr,Xe++,ie||St()}}}();function Mi(Gt){var Xe={},zr=0,_r=0;return{register:function(ie){var St=zr++;ie.onComplete({rethrow:!0,handler:function(rr){return function(){_r--,delete Xe[St]}}})(),Xe[St]=ie,_r++},isEmpty:function(){return _r===0},killAll:function(ie,St){return function(){if(_r===0)return St();var rr=0,Ie={};function Br(yr){Ie[yr]=Xe[yr].kill(ie,function(en){return function(){delete Ie[yr],rr--,Gt.isLeft(en)&&Gt.fromLeft(en)&&setTimeout(function(){throw Gt.fromLeft(en)},0),rr===0&&St()}})()}for(var Xn in Xe)Xe.hasOwnProperty(Xn)&&(rr++,Br(Xn));return Xe={},zr=0,_r=0,function(yr){return new pt(a,function(){for(var en in Ie)Ie.hasOwnProperty(en)&&Ie[en]()})}}}}}var cu=0,_n=1,Xo=2,Jf=3,jf=4,wr=5,Qo=6;function Xf(Gt,Xe,zr){var _r=0,ie=cu,St=zr,rr=null,Ie=null,Br=null,Xn=null,yr=null,en=0,sf=0,xu=null,wi=!0;function Pi(fe){for(var _e,qe,Je;;)switch(_e=null,qe=null,Je=null,ie){case Xo:ie=_n;try{St=Br(St),Xn===null?Br=null:(Br=Xn._1,Xn=Xn._2)}catch(ia){ie=wr,rr=Gt.left(ia),St=null}break;case Jf:Gt.isLeft(St)?(ie=wr,rr=St,St=null):Br===null?ie=wr:(ie=Xo,St=Gt.fromRight(St));break;case _n:switch(St.tag){case i:Br&&(Xn=new pt(c,Br,Xn)),Br=St._2,ie=_n,St=St._1;break;case e:Br===null?(ie=wr,St=Gt.right(St._1)):(ie=Xo,St=St._1);break;case a:ie=Jf,St=Hr(Gt.left,Gt.right,St._1);break;case u:ie=jf,St=So(Gt.left,St._1,function(ia){return function(){_r===fe&&(_r++,zu.enqueue(function(){_r===fe+1&&(ie=Jf,St=ia,Pi(_r))}))}});return;case r:ie=wr,rr=Gt.left(St._1),St=null;break;case n:Br===null?yr=new pt(c,St,yr,Ie):yr=new pt(c,St,new pt(c,new pt(C,Br,Xn),yr,Ie),Ie),Br=null,Xn=null,ie=_n,St=St._1;break;case o:en++,Br===null?yr=new pt(c,St,yr,Ie):yr=new pt(c,St,new pt(c,new pt(C,Br,Xn),yr,Ie),Ie),Br=null,Xn=null,ie=_n,St=St._1;break;case m:ie=Jf,_e=Xf(Gt,Xe,St._2),Xe&&Xe.register(_e),St._1&&_e.run(),St=Gt.right(_e);break;case s:ie=_n,St=Ah(Gt,Xe,St._1);break}break;case wr:if(Br=null,Xn=null,yr===null)ie=Qo,St=Ie||rr||St;else switch(_e=yr._3,Je=yr._1,yr=yr._2,Je.tag){case n:Ie&&Ie!==_e&&en===0?ie=wr:rr&&(ie=_n,St=Je._2(Gt.fromLeft(rr)),rr=null);break;case C:Ie&&Ie!==_e&&en===0||rr?ie=wr:(Br=Je._1,Xn=Je._2,ie=Xo,St=Gt.fromRight(St));break;case o:en--,rr===null&&(qe=Gt.fromRight(St),yr=new pt(c,new pt(ut,Je._2,qe),yr,_e),(Ie===_e||en>0)&&(ie=_n,St=Je._3(qe)));break;case ut:yr=new pt(c,new pt(ve,St,rr),yr,Ie),ie=_n,Ie&&Ie!==_e&&en===0?St=Je._1.killed(Gt.fromLeft(Ie))(Je._2):rr?St=Je._1.failed(Gt.fromLeft(rr))(Je._2):St=Je._1.completed(Gt.fromRight(St))(Je._2),rr=null,en++;break;case mt:en++,yr=new pt(c,new pt(ve,St,rr),yr,Ie),ie=_n,St=Je._1;break;case ve:en--,ie=wr,St=Je._1,rr=Je._2;break}break;case Qo:for(var Jr in xu)xu.hasOwnProperty(Jr)&&(wi=wi&&xu[Jr].rethrow,ae(xu[Jr].handler(St)));xu=null,Ie&&rr?setTimeout(function(){throw Gt.fromLeft(rr)},0):Gt.isLeft(St)&&wi&&setTimeout(function(){if(wi)throw Gt.fromLeft(St)},0);return;case cu:ie=_n;break;case jf:return}}function Vr(fe){return function(){if(ie===Qo)return wi=wi&&fe.rethrow,fe.handler(St)(),function(){};var _e=sf++;return xu=xu||{},xu[_e]=fe,function(){xu!==null&&delete xu[_e]}}}function de(fe,_e){return function(){if(ie===Qo)return _e(Gt.right(void 0))(),function(){};var qe=Vr({rethrow:!1,handler:function(){return _e(Gt.right(void 0))}})();switch(ie){case cu:Ie=Gt.left(fe),ie=Qo,St=Ie,Pi(_r);break;case jf:Ie===null&&(Ie=Gt.left(fe)),en===0&&(ie===jf&&(yr=new pt(c,new pt(mt,St(fe)),yr,Ie)),ie=wr,St=null,rr=null,Pi(++_r));break;default:Ie===null&&(Ie=Gt.left(fe)),en===0&&(ie=wr,St=null,rr=null)}return qe}}function Ne(fe){return function(){var _e=Vr({rethrow:!1,handler:fe})();return ie===cu&&Pi(_r),_e}}return{kill:de,join:Ne,onComplete:Vr,isSuspended:function(){return ie===cu},run:function(){ie===cu&&(zu.isDraining()?Pi(_r):zu.enqueue(function(){Pi(_r)}))}}}function Ko(Gt,Xe,zr,_r){var ie=0,St={},rr=0,Ie={},Br=new Error("[ParAff] Early exit"),Xn=null,yr=t;function en(Vr,de,Ne){var fe=de,_e=null,qe=null,Je=0,Jr={},ia,cl;t:for(;;)switch(ia=null,fe.tag){case ne:if(fe._3===t&&(ia=St[fe._1],Jr[Je++]=ia.kill(Vr,function(kh){return function(){Je--,Je===0&&Ne(kh)()}})),_e===null)break t;fe=_e._2,qe===null?_e=null:(_e=qe._1,qe=qe._2);break;case l:fe=fe._2;break;case v:case D:_e&&(qe=new pt(c,_e,qe)),_e=fe,fe=fe._1;break}if(Je===0)Ne(Gt.right(void 0))();else for(cl=0,ia=Je;cl<ia;cl++)Jr[cl]=Jr[cl]();return Jr}function sf(Vr,de,Ne){var fe,_e,qe,Je,Jr,ia;Gt.isLeft(Vr)?(fe=Vr,_e=null):(_e=Vr,fe=null);t:for(;;){if(qe=null,Je=null,Jr=null,ia=null,Xn!==null)return;if(de===null){_r(fe||_e)();return}if(de._3!==t)return;switch(de.tag){case l:fe===null?(de._3=Gt.right(de._1(Gt.fromRight(_e))),_e=de._3):de._3=fe;break;case v:if(qe=de._1._3,Je=de._2._3,fe){if(de._3=fe,Jr=!0,ia=rr++,Ie[ia]=en(Br,fe===qe?de._2:de._1,function(){return function(){delete Ie[ia],Jr?Jr=!1:Ne===null?sf(fe,null,null):sf(fe,Ne._1,Ne._2)}}),Jr){Jr=!1;return}}else{if(qe===t||Je===t)return;_e=Gt.right(Gt.fromRight(qe)(Gt.fromRight(Je))),de._3=_e}break;case D:if(qe=de._1._3,Je=de._2._3,qe===t&&Gt.isLeft(Je)||Je===t&&Gt.isLeft(qe))return;if(qe!==t&&Gt.isLeft(qe)&&Je!==t&&Gt.isLeft(Je))fe=_e===qe?Je:qe,_e=null,de._3=fe;else if(de._3=_e,Jr=!0,ia=rr++,Ie[ia]=en(Br,_e===qe?de._2:de._1,function(){return function(){delete Ie[ia],Jr?Jr=!1:Ne===null?sf(_e,null,null):sf(_e,Ne._1,Ne._2)}}),Jr){Jr=!1;return}break}Ne===null?de=null:(de=Ne._1,Ne=Ne._2)}}function xu(Vr){return function(de){return function(){delete St[Vr._1],Vr._3=de,sf(de,Vr._2._1,Vr._2._2)}}}function wi(){var Vr=_n,de=zr,Ne=null,fe=null,_e,qe;t:for(;;)switch(_e=null,qe=null,Vr){case _n:switch(de.tag){case l:Ne&&(fe=new pt(c,Ne,fe)),Ne=new pt(l,de._1,t,t),de=de._2;break;case v:Ne&&(fe=new pt(c,Ne,fe)),Ne=new pt(v,t,de._2,t),de=de._1;break;case D:Ne&&(fe=new pt(c,Ne,fe)),Ne=new pt(D,t,de._2,t),de=de._1;break;default:qe=ie++,Vr=wr,_e=de,de=new pt(ne,qe,new pt(c,Ne,fe),t),_e=Xf(Gt,Xe,_e),_e.onComplete({rethrow:!1,handler:xu(de)})(),St[qe]=_e,Xe&&Xe.register(_e)}break;case wr:if(Ne===null)break t;Ne._1===t?(Ne._1=de,Vr=_n,de=Ne._2,Ne._2=t):(Ne._2=de,de=Ne,fe===null?Ne=null:(Ne=fe._1,fe=fe._2))}for(yr=de,qe=0;qe<ie;qe++)St[qe].run()}function Pi(Vr,de){Xn=Gt.left(Vr);var Ne;for(var fe in Ie)if(Ie.hasOwnProperty(fe)){Ne=Ie[fe];for(fe in Ne)Ne.hasOwnProperty(fe)&&Ne[fe]()}Ie=null;var _e=en(Vr,yr,de);return function(qe){return new pt(u,function(Je){return function(){for(var Jr in _e)_e.hasOwnProperty(Jr)&&_e[Jr]();return he}})}}return wi(),function(Vr){return new pt(u,function(de){return function(){return Pi(Vr,de)}})}}function Ah(Gt,Xe,zr){return new pt(u,function(_r){return function(){return Ko(Gt,Xe,zr,_r)}})}return pt.EMPTY=t,pt.Pure=De(e),pt.Throw=De(r),pt.Catch=De(n),pt.Sync=De(a),pt.Async=De(u),pt.Bind=De(i),pt.Bracket=De(o),pt.Fork=De(m),pt.Seq=De(s),pt.ParMap=De(l),pt.ParApply=De(v),pt.ParAlt=De(D),pt.Fiber=Xf,pt.Supervisor=Mi,pt.Scheduler=zu,pt.nonCanceler=he,pt}(),FE=$a.Pure,wO=$a.Throw;function $E(t){return function(e){return e.tag===$a.Pure.tag?$a.Pure(t(e._1)):$a.Bind(e,function(r){return $a.Pure(t(r))})}}function ME(t){return function(e){return $a.Bind(t,e)}}var wE=$a.Sync;function PE(t){return function(e){return $a.ParMap(t,e)}}function OE(t){return function(e){return $a.ParApply(t,e)}}function IE(t){return function(e){return $a.ParAlt(t,e)}}var qc=$a.Async;function RE(t,e){return function(){return $a.Fiber(t,null,e)}}var PO=function(){function t(r,n){return r===0&&typeof setImmediate<"u"?setImmediate(n):setTimeout(n,r)}function e(r,n){return r===0&&typeof clearImmediate<"u"?clearImmediate(n):clearTimeout(n)}return function(r,n){return $a.Async(function(a){return function(){var u=t(n,a(r()));return function(){return $a.Sync(function(){return r(e(n,u))})}}})}}(),NE=$a.Seq;var IO=function(t){return function(e){return function(r){var n=Ao(t),a=vr(t.Applicative1())(e)(function(){var u=Pn(t);return function(i){return u(r(i))}}());return function(u){return n(a(u))}}}},LE=function(t){return function(e){return function(r){var n=Ao(t),a=Wn(e)(t.Applicative1())(function(){var u=Pn(t);return function(i){return u(r(i))}}());return function(u){return n(a(u))}}}},WE=function(t){return function(e){return IO(t)(e)(tt(et))}};var RO=function(t){return t};var GE=function(t){return t};var O_=function(t){return t.toDuration};var UE={fromDuration:$m()()(RO)(function(t){return t*1e3}),toDuration:$m()()(GE)(function(t){return t/1e3})};var qE=function(t,e,r){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return n=1,a=r(),n=2,a}};var LO=function(t){return t};var zc={map:PE},Ai={map:$E};var WO=function(){var t=function(n){if(n instanceof Xt)return n.value0;if(n instanceof jt)return Qa("unsafeFromRight: Left");throw new Error("Failed pattern match at Effect.Aff (line 407, column 21 - line 409, column 54): "+[n.constructor.name])},e=function(n){if(n instanceof jt)return n.value0;if(n instanceof Xt)return Qa("unsafeFromLeft: Right");throw new Error("Failed pattern match at Effect.Aff (line 402, column 20 - line 404, column 55): "+[n.constructor.name])},r=function(n){if(n instanceof jt)return!0;if(n instanceof Xt)return!1;throw new Error("Failed pattern match at Effect.Aff (line 397, column 12 - line 399, column 21): "+[n.constructor.name])};return{isLeft:r,fromLeft:e,fromRight:t,left:jt.create,right:Xt.create}}(),BO=function(t){return RE(WO,t)},ko=function(t){return function(){var r=BO(t)();return r.run(),r}},Ho=function(){var t=Ae(F);return function(e){return t(ko(e))}}();var ki={apply:OE,Functor0:function(){return zc}};var QD={Applicative0:function(){return ya},Bind1:function(){return Lr}},Lr={bind:ME,Apply0:function(){return KD(0)}},ya={pure:FE,Apply0:function(){return KD(0)}},KD=qE("applyAff","Effect.Aff",function(){return{apply:ju(QD),Functor0:function(){return Ai}}}),HE=KD(71);var Nr={liftEffect:wE,Monad0:function(){return QD}},zE=function(){var t=br(Nr);return function(e){return LO(x(t(e)))}}(),VE=function(t){return qc(function(e){return _(F)(zE)(t.join(e))})};var JE=function(t){return function(e){return q(Lr)(br(Nr)(e.isSuspended))(function(r){return r?br(Nr)(Ae(F)(e.kill(t,x(T(f)(void 0))))):qc(function(n){return _(F)(zE)(e.kill(t,n))})})}};var On={parallel:lt,sequential:NE,Monad0:function(){return QD},Applicative1:function(){return GO(0)}},GO=qE("applicativeParAff","Effect.Aff",function(){return{pure:function(){var t=Pn(On),e=T(ya);return function(r){return t(e(r))}}(),Apply0:function(){return ki}}});var UO={append:function(t){return function(e){return function(r){return WE(On)(zt)([t(r),e(r)])}}}};var qO=x(T(ya)(void 0)),jE={mempty:qO,Semigroup0:function(){return UO}};var XE={alt:IE,Functor0:function(){return zc}};var QE=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),I_=function(){function t(){}return t.value=new t,t}(),Uf=function(){function t(){}return t.value=new t,t}(),R_=function(){function t(){}return t.value=new t,t}(),qf=function(){function t(){}return t.value=new t,t}(),N_=function(){function t(){}return t.value=new t,t}(),L_=function(){function t(){}return t.value=new t,t}(),KE=function(){function t(){}return t.value=new t,t}(),Rs=function(){function t(){}return t.value=new t,t}(),Ns=function(){function t(){}return t.value=new t,t}(),W_=function(){function t(){}return t.value=new t,t}(),B_=function(){function t(){}return t.value=new t,t}(),YE=function(){function t(){}return t.value=new t,t}(),Vc=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),YD=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var HO="numeric",zO="sudden",VO="unit",JO="cancel",jO="step",XO="linear",QO="exponential",KO="envelope",ZE=function(t,e,r,n){if(r.type===zO)t.value=r.value.n;else if(r.type===VO)e.id&&ZO(e.id,n),n.units[r.value.i].main.connect(t),e.id=r.value.i;else if(r.type===HO)t[r.value.t.type===jO?"setValueAtTime":r.value.t.type===XO?"linearRampToValueAtTime":r.value.t.type===QO?"exponentialRampToValueAtTime":"linearRampToValueAtTime"](r.value.n,r.value.o);else if(r.type===JO)r.value.hold?t.cancelAndHoldAtTime(r.value.o):t.cancelScheduledValues(r.value.o);else if(r.type===KO){let a=r.value.o;t.cancelScheduledValues(Math.max(0,a)),t.setValueCurveAtTime(r.value.p,a,r.value.d)}else throw new Error("No idea what to do with "+JSON.stringify(r))},YO=function(t,e,r,n,a){return n[r]||(n[r]={}),ZE(e.parameters.get(r),n[r],a,t)},Uu=function(t,e,r,n,a){return n[r]||(n[r]={}),ZE(e[r],n[r],a,t)},hr=function(t,e,r){let n=e.value0?e.value0:"@fan@";r.scopes[n]||(r.scopes[n]=[]),r.scopes[n].push(t),r.units[t].scope=n},Tr=function(t,e){e.toConnect[t]&&(e.toConnect[t].forEach(function(r){r.w?e.units[r.w]?r.f():(e.toConnect[r.w]||(e.toConnect[r.w]=[]),e.toConnect[r.w].push({f:r.f})):r.f()}),delete e.toConnect[t])},xr=function(t,e,r,n){t()(a=>tS(e,a,n))(r)},tS=function(t,e,r){var n=function(){r.units[t].audioOutgoing.push(e),r.units[t].pendingOn||(r.units[t].main.connect(r.units[e].main),r.units[e].se&&r.units[t].main.connect(r.units[e].se))};if(!r.units[t]){r.toConnect[t]||(r.toConnect[t]=[]);var a={f:n};e!==t&&!r.units[e]&&(a.w=e),r.toConnect[t].push(a);return}if(!r.units[e]){r.toConnect[e]||(r.toConnect[e]=[]);var a={f:n};e!==t&&!r.units[t]&&(a.w=t),r.toConnect[e].push(a);return}n()};function ZD(t){return function(e){return function(){delete e.units[t.id]}}}function td(t){return function(e){return function(){tS(t.from,t.to,e)}}}var ZO=function(t,e){if(e.units[t].scope==="@fan@")return;let r=e.units[t].scope;e.scopes[r].forEach(n=>{delete e.units[n]}),delete e.scopes[r]};function ed(t){return function(e){return function(){var r=t.from,n=t.to;if(e.units[r].audioOutgoing=e.units[r].audioOutgoing.filter(function(u){return u!==n}),e.units[r].main.disconnect(e.units[n].main),e.units[n].se&&e.units[r].main.disconnect(e.units[n].se),e.units[r].scope==="@fan@")return;let a=e.units[r].scope;e.scopes[a].forEach(u=>{delete e.units[u]}),delete e.scopes[a]}}}var rd=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"allpass",Q:e.q,frequency:e.frequency})},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},nd=t=>e=>r=>()=>{var n=e.id,a=e.cb,u=new AnalyserNode(r.context,e),i=a(u)();r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],analyserOrig:a,analyser:i,main:r.context.createGain(),se:u},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},ad=t=>e=>r=>()=>{var n=e.id,a=e.options;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new AudioWorkletNode(r.context,a.name,{numberOfInputs:a.numberOfInputs,numberOfOutputs:a.numberOfOutputs,outputChannelCount:a.outputChannelCount,parameterData:a.parameterData,processorOptions:a.processorOptions})},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},ud=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"bandpass",Q:e.q,frequency:e.frequency})},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},od=t=>e=>r=>()=>{var n=e.id,a=function(i,o){return new ConstantSourceNode(i,o)},u={offset:e.offset};r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},id=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new ConvolverNode(r.context,{buffer:e.buffer})},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},fd=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DelayNode(r.context,{delayTime:e.delayTime,maxDelayTime:e.maxDelayTime})},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},cd=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DynamicsCompressorNode(r.context,{knee:e.knee,ratio:e.ratio,threshold:e.threshold,attack:e.attack,release:e.release})},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},ld=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new GainNode(r.context,{gain:e.gain})},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},_d=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"highpass",Q:e.q,frequency:e.frequency})},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},pd=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"highshelf",frequency:e.frequency,gain:e.gain})},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},sd=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new IIRFilterNode(r.context,{feedforward:e.feedforward,feedback:e.feedback})},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},md=t=>e=>r=>()=>{var n=e.id,a=function(i,o){return new AudioBufferSourceNode(i,o)},u={loop:!0,buffer:e.buffer,loopStart:e.loopStart,loopEnd:e.loopEnd,playbackRate:e.playbackRate};r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},vd=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"lowpass",Q:e.q,frequency:e.frequency})},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},Dd=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"lowshelf",frequency:e.frequency,gain:e.gain})},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},dd=t=>e=>r=>()=>{var n=e.id,a=e.element,u=function(){var i=r.context.createMediaElementSource(a);return i};r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],createClosure:u,resumeClosure:{},main:u()},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},bd=t=>e=>r=>()=>{var n=e.id;r.units[e.id]={main:r.context.createMediaStreamSource(e.microphone),controllers:{},audioOutgoing:[],controlOutgoing:[]},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},yd=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"notch",frequency:e.frequency,Q:e.q})},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},Ad=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"peaking",frequency:e.frequency,Q:e.q,gain:e.gain})},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},kd=t=>e=>r=>()=>{var n=e.id,a=function(i,o){var m={frequency:o.frequency,periodicWave:o.spec.type==="wave"?o.spec.value:ub(r.context)(o.spec.value.real)(o.spec.value.img)()},s=new OscillatorNode(i,m);return s},u={frequency:e.frequency,type:"custom",spec:e.spec};r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},gd=t=>e=>r=>()=>{var n=e.id,a=function(i,o){var m={loop:o.loop,buffer:o.buffer,playbackRate:o.playbackRate};return new AudioBufferSourceNode(i,m)},u={loop:!1,buffer:e.buffer,playbackRate:e.playbackRate,bufferOffset:e.bufferOffset,duration:t(void 0)(i=>i)(e.duration)};r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},Cd=t=>e=>r=>()=>{var n=e.id,a=e.cb,u=r.context.createMediaStreamDestination(),i=new MediaRecorder(u.stream);a(i)(),i.start(),r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],recorderOrig:a,recorder:i,main:r.context.createGain(),se:u},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},Ed=t=>e=>r=>()=>{var n=e.id,a=function(i,o){return new OscillatorNode(i,o)},u={frequency:e.frequency,type:"sawtooth"};r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},Sd=t=>e=>r=>()=>{var n=e.id,a=function(i,o){return new OscillatorNode(i,o)},u={frequency:e.frequency,type:"sine"};r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},hd=t=>e=>()=>{e.units[t.id]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:e.context.createGain(),se:e.context.destination}},Td=t=>e=>r=>()=>{var n=e.id;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new StereoPannerNode(r.context,{pan:e.pan})},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},xd=t=>e=>r=>()=>{var n=e.id,a=function(i,o){return new OscillatorNode(i,o)},u={frequency:e.frequency,type:"square"};r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},Fd=t=>e=>r=>()=>{var n=e.id,a=function(i,o){return new OscillatorNode(i,o)},u={frequency:e.frequency,type:"triangle"};r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)},$d=t=>e=>r=>()=>{var n=e.id,a=e.curve,u=e.oversample;r.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new WaveShaperNode(r.context,{curve:a,oversample:u.type})},hr(n,e.scope,r),Tr(n,r),xr(t,n,e.parent,r)};function Md(t){return function(e){return function(){var r=t.id,n=t.cb;e.units[r].analyserOrig!==n&&(e.units[r].analyser&&e.units[r].analyser(),e.units[r].analyser=n(e.units[r].se)(),e.units[r].analyserOrig=n)}}}function wd(t){return function(e){return function(){var r=t.cb,n=t.id;if(e.units[n].recorderOrig!==r){e.units[n].recorder&&e.units[n].recorder.stop();var a=r;e.units[n].recorderOrig=r;var u=new MediaRecorder(e.units[n].se);a(u)(),u.start()}}}}function Pd(t){return function(e){return function(){var r=t.id,n=t.curve;e.units[r].main.curve=n}}}function Od(t){return function(e){return function(){var r=t.id,n=t.paramName,a=t.paramValue;YO(e,e.units[r].main,n,e.units[r].controllers,a)}}}var qu=function(t,e,r){e.resume&&t.value.n!==void 0&&(e.resume[r]=t.value.n)};function Id(t){return function(e){return function(){var r=t.id,n=t.gain;Uu(e,e.units[r].main,"gain",e.units[r].controllers,n),qu(n,e.units[r],"gain")}}}function Rd(t){return function(e){return function(){var r=t.id,n=t.q;Uu(e,e.units[r].main,"Q",e.units[r].controllers,n),qu(n,e.units[r],"Q")}}}function Nd(t){return function(e){return function(){var r=t.id,n=t.buffer;e.units[r].resume&&(e.units[r].resume.buffer=n)}}}function Ld(t){return function(e){return function(){var r=t.id,n=t.buffer;e.units[r].main.buffer=n}}}function Wd(t){return function(e){return function(){var r=t.id,n=t.spec;e.units[r].resume&&(e.units[r].resume.spec=n)}}}function Bd(t){return function(e){return function(){var r=t.id,n=t.pan;Uu(e,e.units[r].main,"pan",e.units[r].controllers,n),qu(n,e.units[r],"pan")}}}function Gd(t){return function(e){return function(){var r=t.id,n=t.threshold;Uu(e,e.units[r].main,"threshold",e.units[r].controllers,n),qu(n,e.units[r],"threshold")}}}function Ud(t){return function(e){return function(){var r=t.id,n=t.loopStart;e.units[r].main.loopStart=n,e.units[r].resume.loopStart=n}}}function qd(t){return function(e){return function(){var r=t.id,n=t.loopEnd;e.units[r].main.loopEnd=n,e.units[r].resume.loopEnd=n}}}function Hd(t){return function(e){return function(){var r=t.id,n=t.bufferOffset;e.units[r].resume.bufferOffset=n}}}function zd(t){return function(e){return function(r){return function(){var n=e.id,a=e.duration;r.units[n].duration=t(void 0)(u=>u)(a)}}}}function Vd(t){return function(e){return function(){var r=t.id,n=t.release;Uu(e,e.units[r].main,"release",e.units[r].controllers,n),qu(n,e.units[r],"release")}}}function Jd(t){return function(e){return function(){var r=t.id,n=t.offset;Uu(e,e.units[r].main,"offset",e.units[r].controllers,n),qu(n,e.units[r],"offset")}}}function jd(t){return function(e){return function(){var r=t.id,n=t.ratio;Uu(e,e.units[r].main,"ratio",e.units[r].controllers,n),qu(n,e.units[r],"ratio")}}}function Xd(t){return function(e){return function(){var r=t.id,n=t.attack;Uu(e,e.units[r].main,"attack",e.units[r].controllers,n),qu(n,e.units[r],"attack")}}}function Qd(t){return function(e){return function(){var r=t.id,n=t.knee;Uu(e,e.units[r].main,"knee",e.units[r].controllers,n),qu(n,e.units[r],"knee")}}}function Kd(t){return function(e){return function(){var r=t.id,n=t.delayTime;Uu(e,e.units[r].main,"delayTime",e.units[r].controllers,n),qu(n,e.units[r],"delayTime")}}}function Yd(t){return function(e){return function(){var r=t.id,n=t.playbackRate;Uu(e,e.units[r].main,"playbackRate",e.units[r].controllers,n),qu(n,e.units[r],"playbackRate")}}}function Zd(t){return function(e){return function(){var r=t.id,n=t.frequency;Uu(e,e.units[r].main,"frequency",e.units[r].controllers,n),qu(n,e.units[r],"frequency")}}}function tb(t){return function(e){return function(){var r=t.id,n=t.onOff;n.x.type==="on"?tI(r)(n)(e)():n.x.type==="off"&&eI(r)(n)(e)()}}}var tI=function(t){return function(e){return function(r){return function(){if(!r.units[t].onOff){r.units[t].pendingOn=!1,r.units[t].onOff=!0,r.units[t].main=r.units[t].createClosure(r.context,r.units[t].resume);for(var n=0;n<r.units[t].audioOutgoing.length;n++){var a=r.units[t].audioOutgoing[n];r.units[t].main.connect(r.units[a].main),r.units[a].se&&r.units[t].main.connect(r.units[a].se)}r.units[t].resume&&r.units[t].resume.bufferOffset?typeof r.units[t].resume.duration=="number"?r.units[t].main.start(r.deprecatedWriteHead+e.o,r.units[t].resume.bufferOffset,r.units[t].resume.duration):r.units[t].main.start(r.deprecatedWriteHead+e.o,r.units[t].resume.bufferOffset):r.units[t].resume&&r.units[t].resume.loopStart?r.units[t].main.start(r.deprecatedWriteHead+e.o,r.units[t].resume.loopStart):r.units[t].main.start(r.deprecatedWriteHead+e.o)}}}}},eI=function(t){return function(e){return function(r){return function(){if(!!r.units[t].onOff){r.units[t].onOff=!1;var n=r.units[t].main;n.addEventListener("ended",()=>{n.disconnect()}),n.stop(r.deprecatedWriteHead+e.o)}}}}};function eb(t){for(var e=new Float32Array(t.length),r=0;r<t.length;r++)e[r]=t[r];return e}function Ls(t){return function(){t.stop()}}function rb(t){return function(e){return function(r){return function(){var n=[];r.ondataavailable=function(a){n.push(a.data)},r.onstop=function(){var a=new Blob(n,{type:t});e(a)(),n=null}}}}}function nb(t){return function(e){return function(){return navigator.mediaDevices.getUserMedia({audio:t,video:e})}}}function G_(t){return function(){var e=new Uint8Array(t.frequencyBinCount);return t.getByteFrequencyData(e),e}}function ab(t){return function(){var e=t.createConstantSource();return e.offset.value=0,e.connect(t.destination),e.start(),function(){e.stop(),e.disconnect(t.destination)}}}var ub=function(t){return function(e){return function(r){return function(){for(var n=new Float32Array(e.length),a=new Float32Array(r.length),u=0;u<e.length;u++)n[u]=e[u];for(var u=0;u<r.length;u++)a[u]=r[u];return t.createPeriodicWave(n,a,{disableNormalization:!0})}}}};function nf(t){return function(){return{context:t,deprecatedWriteHead:0,units:{},scopes:{},unsu:{},toConnect:{}}}}function ob(t){return function(){t.close()}}function ib(t){return function(){return fetch(t).then(function(e){return e.arrayBuffer()},function(e){return console.error("Error fetching buffer",e),Promise.reject(e)})}}function fb(t){return function(e){return function(){return t.decodeAudioData(e)}}}function cb(){return new(window.AudioContext||window.webkitAudioContext)}function lb(t){return function(){return t.state}}function U_(t){return function(){return t.currentTime}}function eS(t){return function(e){return function(r){return function(){t.then(r,e)}}}}var aI=function(t){return function(e){return qc(function(r){return Z_(F)(Pt(jE))(eS(e)(function(n){return r(jt.create(t(n)))()})(function(n){return r(Xt.create(n))()}))})}};var uI=function(t){return Ra(function(e){return Lo("Promise failed, couldn't extract JS Error or String")})(tt(et))(pD(N(_D(Jm)(Qu))(yD(Qu)("Error")(t))(_(b_(To))(Lo)(AD(Qu)(t)))))},rS=aI(uI),Ws=function(t){return q(Lr)(br(Nr)(t))(rS)};function _b(t){return function(){return URL.createObjectURL(t)}}var nS=function(t){return function(e){return function(r){return xt(rb(t))(r)(function(){var n=Qn(hn)(e);return function(a){return n(_b(a))}}())}}};var Hf={ids:_(F)(Ht(ep))(Bu),deleteFromCache:ZD,disconnectXFromY:ed,connectXToY:td,makeAllpass:rd(Yt),makeAnalyser:nd(Yt),makeAudioWorkletNode:ad(Yt),makeBandpass:ud(Yt),makeConstant:od(Yt),makeConvolver:id(Yt),makeDelay:fd(Yt),makeDynamicsCompressor:cd(Yt),makeGain:ld(Yt),makeHighpass:_d(Yt),makeHighshelf:pd(Yt),makeIIRFilter:sd(Yt),makeLoopBuf:md(Yt),makeLowpass:vd(Yt),makeLowshelf:Dd(Yt),makeMediaElement:dd(Yt),makeMicrophone:bd(Yt),makeNotch:yd(Yt),makePeaking:Ad(Yt),makePeriodicOsc:kd(Yt),makePlayBuf:gd(Yt),makeRecorder:Cd(Yt),makeSawtoothOsc:Ed(Yt),makeSinOsc:Sd(Yt),makeSpeaker:hd,makeSquareOsc:xd(Yt),makeStereoPanner:Td(Yt),makeTriangleOsc:Fd(Yt),makeWaveShaper:$d(Yt),setAnalyserNodeCb:Md,setMediaRecorderCb:wd,setWaveShaperCurve:Pd,setAudioWorkletParameter:Od,setBuffer:Nd,setConvolverBuffer:Ld,setDuration:zd(Yt),setPeriodicOsc:Wd,setOnOff:tb,setBufferOffset:Hd,setLoopStart:Ud,setLoopEnd:qd,setRatio:jd,setOffset:Jd,setAttack:Xd,setGain:Id,setQ:Rd,setPan:Bd,setThreshold:Gd,setRelease:Vd,setKnee:Qd,setDelay:Kd,setPlaybackRate:Yd,setFrequency:Zd},Et=function(t){return function(e){return q(Lr)(Ws(ib(e)))(function(){var r=fb(t);return function(n){return Ws(r(n))}}())}},q_=function(t){var e=br(t);return function(r){return e(lb(r))}};var ua=function(t){return br(t)(cb)},Hu=function(t){var e=br(t);return function(r){return e(ab(r))}},Cn=function(t){return function(e){return br(t)(function(){var n=q_(fr)(e)();return Rn(f)(n!=="closed")(ob(e))()})}},lI=lt,_I=lt,Bs=function(t){return function(e){return _(Ai)(function(r){return{microphone:function(){return t?T(ho)(lI(r)):V.value}(),camera:function(){return e?T(ho)(_I(r)):V.value}()}})(Ws(nb(t)(e)))}};var zo=function(){function t(){}return t.value=new t,t}(),Vo=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Tu=function(){function t(){}return t.value=new t,t}(),ln=XD,gi=function(t){return Ao(On)(N(XE)(Pn(On)(q(Lr)(VE(t))(br(Nr))))(Pn(On)(JE(Lo("We navigated away from the page"))(t))))},Jc=function(t){return function(e){return function(r){return function(n){return N(t)(Y(e)(Tu.value))(n)}}}},Ma=function(t){return function(e){return function(r){return function(n){return N(t)(Y(e)(Z(dr)(mr.value)(Ye(x(n)))))(_(t.Functor0())(function(a){return Z(dr)(mr.value)(Ye(x(X(nt)(a)(n))))})(_(t.Functor0())(function(a){return a.value0})(r)))}}}},Gs=function(t){return function(e){return function(r){return function(n){return function(a){return function(u){return function(i){return function(o){return function(m){return _(t)(function(s){return Z(dr)(mr.value)(Ye(x(function(){if(s.value0 instanceof zo)return T(f)(void 0);if(s.value0 instanceof Vo)return X(nt)(X(nt)(s.value0.value0)(n(T(f)(void 0))))(a(Tu.value));if(s.value0 instanceof Tu)return function(){s.value1(),a(zo.value)();var v=ko(q(Lr)(ua(Nr))(function(D){return q(Lr)(Hu(Nr)(D))(function(c){return q(Lr)(u(D))(function(C){return br(Nr)(function(){var mt=i(D)(C)(),ve=X(nt)(X(nt)(mt)(c))(Cn(fr)(D));return a(new Vo(ve))(),ve})})})}))();return ar(e)(hn)(n(function(){return a(Tu.value)(),Ho(gi(v))()}))(function(){return T(f)(void 0)})()};throw new Error("Failed pattern match at WAGS.Example.Docs.Util (line 48, column 21 - line 66, column 26): "+[s.value0.constructor.name])}())))})($n(r)(N(r.Plus0().Alt0())(Y(r)(T(f)(void 0)))(_(t)(function(s){return s.value0})(o)))(_(t)(at.create)(m)))}}}}}}}}},wa=function(t){return function(e){return function(r){return function(){return t(r)(),e(new QE(r))()}}}},Us=function(t){return function(e){return function(r){return function(n){return function(a){return Gr(k)(function(u){return function(i){var o=Jc(I(f))(h(k))(e)(i);return bc(p)(N(I(f))(Y(h(k))(Z(qp)(Vt.value)("cursor: pointer;")))(Gs(g)(je)(h(k))(r)(u)(n)(a)(e)(o)))([un(ue)(_(g)(function(m){if(m instanceof Tu)return t;if(m instanceof zo)return"\u23F3";if(m instanceof Vo)return"\u{1F6D1}";throw new Error("Failed pattern match at WAGS.Example.Docs.Util (line 115, column 19 - line 118, column 37): "+[m.constructor.name])})(o))])}})}}}}},ht=function(t){return function(e){return function(r){return function(n){return Gr(k)(function(a){return function(u){var i=Jc(I(f))(h(k))(t)(u);return gn(p)(Gs(g)(je)(h(k))(e)(a)(r)(n)(t)(i))([un(ue)(_(g)(function(o){if(o instanceof Tu)return"Turn on";if(o instanceof zo)return"Loading...";if(o instanceof Vo)return"Turn off";throw new Error("Failed pattern match at WAGS.Example.Docs.Util (line 89, column 19 - line 92, column 44): "+[o.constructor.name])})(i))])}})}}}};var jc=function(t){return function(e){return function(){var n=nf(t)(),a=$t(Bf([new Sf(_(g)(function(u){return Ef.create(YA(u))})(e))])(Hf))(function(u){return u(n)})();return a}}};var Dt=function(t){return function(e){return function(){var n=nf(t)(),a=$t(Bf(e)(Hf))(function(u){return u(n)})();return a}}},qs=function(t){return function(){var r=ua(fr)();return _(F)(function(n){return X(nt)(n)(Cn(fr)(r))})(Dt(r)(t))()}};var pI=function(){return d.value}(),aS=function(t){return function(e){return function(r){return pe({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(p)(W()(K(f))({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}})(p))(d.value)(pI)({allpass:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([Fa(le(Ce)(a)(_t()))(function(u){return function(i){return Ot(ft)(.2)([u,M_(HD)(700)([M_(xs(Ct(gt()(J(J(kt)(MD)()()()({reflectSymbol:function(){return"q"}}))(hs)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:990,q:20})([u]),M_(HD)(1110)([u,M_(xs(Ct(gt()(J(J(kt)(MD)()()()({reflectSymbol:function(){return"q"}}))(hs)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:2010,q:30})([u])])])])}})])}})))})}}};function Ci(t){return function(r,n,a){if(n===null)return new t(r);var u=r.byteLength,i=t.BYTES_PER_ELEMENT,o=Math.min(u,n>>>0);if(a===null)return new t(r,o);var m=Math.min((u-o)/i,a);return new t(r,o,m)}}var mI=Ci(Uint8ClampedArray),vI=Ci(Uint32Array),DI=Ci(Uint16Array),uS=Ci(Uint8Array),dI=Ci(Int32Array),bI=Ci(Int16Array),yI=Ci(Int8Array),AI=Ci(Float32Array),kI=Ci(Float64Array);function oS(t){for(var e=t.length,r=new Array(e),n=0;n<e;n++)r[n]=t[n];return r}var Hs={create:uS,BinaryValue0:function(){}};var zs=function(t){return function(e){return function(){return oS(e)}}};var Xc=ku,Qc=ku,Kc=ku,ru=ku,nu=ku,au=ku,uu=ku,ou=ku;function Vs(t){return t|0}var Ei=function(){return window};function lS(t,e,r,n){if(typeof window<"u"){var a=window[r];if(a!=null&&n instanceof a)return e(n)}for(var u=n;u!=null;){var i=Object.getPrototypeOf(u),o=i.constructor.name;if(o===r)return e(n);if(o==="Object")return t;u=i}return t}var It=function(t){return function(e){return lS(V.value,B.create,t,e)}};var pb=It("HTMLCanvasElement");function _S(t){return function(){return t.body}}var pS=function(){var t=_(F)(Zr);return function(e){return t(_S(e))}}();var sS=lt;function zf(t){return function(){return t.valueAsNumber}}var Yc=It("HTMLInputElement");function mb(t){return function(){return t.document}}function Js(t){return function(e){return function(){return e.requestAnimationFrame(t)}}}var vb=lt;var fR=function(t,e,r){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return n=1,a=r(),n=2,a}},af=Wt(function(t){return function(){var r=Ei(),n=sr(!0)(),a=fR("fx","FRP.Event.Animate",function(){return Ae(F)(xt(Js)(r)(function(){var o=Rr(n)();return Rn(f)(o)(function(){return t(void 0)(),a(19)()})()}))}),u=a(15);return u(),Tn(!1)(n)}});var cR="background-color: rgb(150,30,10);",lR="background-color: rgb(130,60,10);",_R="background-color: rgb(80,90,10);",pR="background-color: rgb(10,130,10);",sR="background-color: rgb(10,100,0);",mR=ps(Za)(function(t){return Pe(we(ls)()(qa)()(D_))(cR)(Pe(we(aa)()(vn)()(qa))(lR)(Pe(we(gu)()(Dn)()(vn))(_R)(Pe(we(Cu)()(dn)()(Dn))(pR)(Pe(we(Eu)()(Su)()(dn))(sR)(Gu)))))}),vR=function(t){return function(e){return function(r){return function(n){return Ps(ws(Ct(gt()(J(J(kt)(Ms)()()()({reflectSymbol:function(){return"fftSize"}}))(t)()()()({reflectSymbol:function(){return"cb"}})))(At()())))({cb:n,fftSize:As.value})([le(e)(r)(_t())])}}}},DR=function(){return d.value}(),Re="background-color: rgb(255,255,255,0.0);",Le=function(t){return function(e){return function(r){return function(n){return function(a){return function(u){return function(i){return function(o){return function(m){return function(s){return function(l){return _(t)(function(v){var D=d_(e)()(d_(n)()(v)(m))(s);return D?Z(u)(Vt.value)(d_(e)()(d_(n)()(mR)(m))(s)):Z(u)(Vt.value)(Re)})(l)}}}}}}}}}}},dR=function(){return 15/40}(),bR=function(){return 10/40}(),yR=function(){return 7/40}(),AR=function(){return 3/40}(),kR=function(){return 1/40}(),vS=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
  <h2 id="analyser">Analyser</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode">analyser node</a> provides methods to recuperate the analysed data of an input. This is how, for example, Google Meet shows the little animation around a microphone icon. Wags provides the possibility to use the analyser as the terminus of an audio graph <i>or</i> as part of a longer DSP chain, as in the following example. The example uses an FFT size of 256, which is indicated in Wags as <code>TTT8</code> (two to the eighth power).</p>

  <pre><code>analyser_ { cb, fftSize: TTT8 } [ loopBuf atar bangOn ]</code></pre>

  ~analyser~
  </section>
`}})()()(W()(K(f))({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}})(p))(p)(DR)({analyser:L(rt(Gr(k)(function(n){return function(a){var u=Ll(bu(f))(tt(et))(a),i=Jc(I(f))(h(k))(r)(function(m){return m.right}(u)),o=function(m){return m.left}(u);return Ue(p)([gn(p)(N(I(f))(Y(h(k))(Z(Dc)(Vt.value)("cursor: pointer;")))(Gs(g)(je)(h(k))(t)(function(m){return n(Xt.create(m))})(function(m){return Et(m)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(m){return function(s){return function(){var v=sr(V.value)(),D=nf(m)(),c=Bf([vR($s)(Ce)(s)(function(ut){return function(){return Tn(new B(ut))(v)(),Tn(V.value)(v)}})])(Hf),C=$t(N(I(f))(_(g)(Xt.create)(c))(_(g)(jt.create)(af)))(function(ut){if(ut instanceof Xt)return ut.value0(D);if(ut instanceof jt)return function(){var ve=Rr(v)();return _a(f)(Ke)(ve)(function(ne){return function(){var lr=G_(ne)(),pt=zs(Hs)(lr)(),De=sr(0)(),he=sr(0)(),ae=sr(0)(),Hr=sr(0)(),So=sr(0)(),zu=sr(0)(),Mi=sr(0)(),cu=sr(0)(),_n=sr(0)(),Xo=sr(0)(),Jf=function(wr){if(wr<32)return De;if(wr<64)return he;if(wr<96)return ae;if(wr<128)return Hr;if(wr<168)return So;if(wr<160)return zu;if(wr<224)return Mi;if(nr)return cu;throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Analyser (line 143, column 45 - line 151, column 63): "+[wr.constructor.name])};Al(pt)(function(wr){var Qo=Vs(wr);return function(){var Ko=Rr(Xo)();return Af(We(Fu)(Qo))(_n)(),Af(We(Fu)(Qo))(Jf(Ko))(),Af(We(Fu)(1))(Xo)()}})();var jf=Wn(wg)(f)(function(wr){return function(){var Xf=_(F)(ze)(Rr(wr))(),Ko=_(F)(Ku(yl)(Xf))(_(F)(ze)(Rr(_n)))();return Pe(we(ls)()(qa)()(D_))(Ko>dR)(Pe(we(aa)()(vn)()(qa))(Ko>bR)(Pe(we(gu)()(Dn)()(vn))(Ko>yR)(Pe(we(Cu)()(dn)()(Dn))(Ko>AR)(Pe(we(Eu)()(Su)()(dn))(Ko>kR)(Gu)))))}})(Pe(we(xg)()(uD)()(Mg))(De)(Pe(we(Fg)()(oD)()(uD))(he)(Pe(we($g)()(D_)()(oD))(ae)(Pe(we(ls)()(qa)()(D_))(Hr)(Pe(we(aa)()(vn)()(qa))(So)(Pe(we(gu)()(Dn)()(vn))(zu)(Pe(we(Cu)()(dn)()(Dn))(Mi)(Pe(we(Eu)()(Su)()(dn))(cu)(Gu)))))))))();return n(new jt(jf))()}})()};throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Analyser (line 124, column 57 - line 162, column 57): "+[ut.constructor.name])})();return function(){return C(),function(){var ve=q_(fr)(m)();return Rn(f)(ve!=="closed")(Cn(fr)(m))()}(),n(new jt(ps(Za)(x(ps(xa)(x(!1))))))()}}}})(r)(i)))([un(ue)(_(g)(function(m){if(m instanceof Tu)return"Turn on";if(m instanceof zo)return"Loading...";if(m instanceof Vo)return"Turn off";throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Analyser (line 175, column 31 - line 178, column 56): "+[m.constructor.name])})(i))]),Te(p)(Y(h(k))(Z(st)(Vt.value)("display: grid; grid-template-columns: repeat(8, 1fr); grid-auto-rows: 20px;")))([Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Mr)(ba)(Mr)(bo)(st)(ba)(bo)(ou)(ou)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Mr)(ba)(zn)(Do)(st)(ba)(Do)(uu)(ou)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Mr)(ba)(Hn)(vo)(st)(ba)(vo)(au)(ou)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Mr)(ba)(qn)(mo)(st)(ba)(mo)(nu)(ou)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Mr)(ba)(Un)(so)(st)(ba)(so)(ru)(ou)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Mr)(ba)(xa)(po)(st)(ba)(po)(Kc)(ou)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Mr)(ba)(Bo)(_o)(st)(ba)(_o)(Qc)(ou)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Mr)(ba)(Wo)(lo)(st)(ba)(lo)(Xc)(ou)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(zn)(da)(Mr)(bo)(st)(da)(bo)(ou)(uu)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(zn)(da)(zn)(Do)(st)(da)(Do)(uu)(uu)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(zn)(da)(Hn)(vo)(st)(da)(vo)(au)(uu)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(zn)(da)(qn)(mo)(st)(da)(mo)(nu)(uu)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(zn)(da)(Un)(so)(st)(da)(so)(ru)(uu)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(zn)(da)(xa)(po)(st)(da)(po)(Kc)(uu)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(zn)(da)(Bo)(_o)(st)(da)(_o)(Qc)(uu)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(zn)(da)(Wo)(lo)(st)(da)(lo)(Xc)(uu)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Hn)(Da)(Mr)(bo)(st)(Da)(bo)(ou)(au)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Hn)(Da)(zn)(Do)(st)(Da)(Do)(uu)(au)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Hn)(Da)(Hn)(vo)(st)(Da)(vo)(au)(au)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Hn)(Da)(qn)(mo)(st)(Da)(mo)(nu)(au)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Hn)(Da)(Un)(so)(st)(Da)(so)(ru)(au)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Hn)(Da)(xa)(po)(st)(Da)(po)(Kc)(au)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Hn)(Da)(Bo)(_o)(st)(Da)(_o)(Qc)(au)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Hn)(Da)(Wo)(lo)(st)(Da)(lo)(Xc)(au)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(qn)(va)(Mr)(bo)(st)(va)(bo)(ou)(nu)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(qn)(va)(zn)(Do)(st)(va)(Do)(uu)(nu)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(qn)(va)(Hn)(vo)(st)(va)(vo)(au)(nu)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(qn)(va)(qn)(mo)(st)(va)(mo)(nu)(nu)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(qn)(va)(Un)(so)(st)(va)(so)(ru)(nu)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(qn)(va)(xa)(po)(st)(va)(po)(Kc)(nu)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(qn)(va)(Bo)(_o)(st)(va)(_o)(Qc)(nu)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(qn)(va)(Wo)(lo)(st)(va)(lo)(Xc)(nu)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Un)(ma)(Mr)(bo)(st)(ma)(bo)(ou)(ru)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Un)(ma)(zn)(Do)(st)(ma)(Do)(uu)(ru)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Un)(ma)(Hn)(vo)(st)(ma)(vo)(au)(ru)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Un)(ma)(qn)(mo)(st)(ma)(mo)(nu)(ru)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Un)(ma)(Un)(so)(st)(ma)(so)(ru)(ru)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Un)(ma)(xa)(po)(st)(ma)(po)(Kc)(ru)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Un)(ma)(Bo)(_o)(st)(ma)(_o)(Qc)(ru)(o)))([]),Te(p)(N(I(f))(Y(h(k))(Z(st)(Vt.value)(Re)))(Le(g)(Un)(ma)(Wo)(lo)(st)(ma)(lo)(Xc)(ru)(o)))([])])])}})))})}}};var CR=function(){return d.value}(),DS=function(t){return function(e){return function(r){return pe({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(p)(W()(K(f))({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}})(p))(d.value)(CR)({bandpass:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([Fa(le(Ce)(a)(_t()))(function(u){return function(i){return Ot(ft)(.8)([kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:400,q:1})([u]),kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:880,q:5})([u]),kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:1200,q:10})([u]),kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:2e3,q:20})([u]),kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:3e3,q:30})([u])])}})])}})))})}}};var SR=function(){return d.value}(),dS=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
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
`}})()()(W()(K(f))({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}})(p))(p)(SR)({compression:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([CE(lE(Ct(gt()(kt))(At()())))({})([le(Ce)(a)(_t())])])}})))})}}};var oa=function(){return function(t){var e=nn(),r=ir()({reflectSymbol:function(){return"playbackRate"}})(d.value),n=Nf(t);return function(a){return e(r(n(a)))}}},Vf=function(){return function(t){var e=nn(),r=ir()({reflectSymbol:function(){return"onOff"}})(d.value),n=yC(t);return function(a){return e(r(n(a)))}}},bS=function(){return function(t){var e=nn(),r=ir()({reflectSymbol:function(){return"offset"}})(d.value),n=Nf(t);return function(a){return e(r(n(a)))}}},yS=function(){var t=nn(),e=ir()({reflectSymbol:function(){return"loopStart"}})(d.value);return function(r){return t(e(r))}},AS=function(){var t=nn(),e=ir()({reflectSymbol:function(){return"loopEnd"}})(d.value);return function(r){return t(e(r))}},En=function(){return function(t){var e=nn(),r=ir()({reflectSymbol:function(){return"gain"}})(d.value),n=Nf(t);return function(a){return e(r(n(a)))}}},go=function(){return function(t){var e=nn(),r=ir()({reflectSymbol:function(){return"frequency"}})(d.value),n=Nf(t);return function(a){return e(r(n(a)))}}};var Zc=function(){return function(t){var e=nn(),r=ir()({reflectSymbol:function(){return"delayTime"}})(d.value),n=Nf(t);return function(a){return e(r(n(a)))}}};var TR=function(){return d.value}(),kS=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
  <h2 id="constant">Constant value</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode">Constant values</a>, or DC offset, is a way to output an unchanging stream of values. This is only really useful when testing the performance of speakers or microphones and/or when working with a custom audio node that supports constant streaming values. Note that the constant source node in the web audio API can <i>also</i> be used to control audio parameters. Wags uses this feature of constant nodes under the hood to optimize certain computations.</p>

  <p>The following example abuses a constant audio node by turning it into a gnarly inpulse generator. We'll learn about the tie fighter symbol <code>~tf~</code> and the <code>bang</code> in the next section on Events. Kids, don't try this at home!</p>

  <pre><code>~txt~</code></pre>

  ~constant~
  </section>
`}})()()(W()(W()(W()(K(f))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(p))({reflectType:function(){return"tf"}})({reflectSymbol:function(){return"tf"}})(p))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}})(p))(p)(TR)({tf:L(or(ue)("<|>")),txt:L(or(ue)(`run2_
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
  ]`)),constant:L(rt(ht(r)(t)(function(n){return T(ya)(void 0)})(function(n){return function(a){return Dt(n)([Ot(ft)(.5)([Is(Ss)(0)(N(I(f))(_t())(Y(h(k))(bS()(wn)({d:5,o:.1,p:eo(Gi)(function(u){return x(function(){var i=su(xo)(u)(3)===0;return i?1:0}())})(pn(0)(1920))}))))])])}})))})}}};var FR=function(){return d.value}(),gS=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
  <h2 id="convolution">Convolution</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConvolverNode">Convolution</a>, aka reverb, is a way to graft the shape of one sound (usually an <a href="https://en.wikipedia.org/wiki/Impulse_response">impulse response</a>) onto another. Convolution can sound great, but it is a <i>very expensive operation</i> that will cause noticeable artifacts on low-end devices. When shipping audio code to production, you're usually better off using an Audio Worklet Node with reverb optimized for your specific case. That said, for PoCs or hobbyist projects, convolution is great!</p>

  <pre><code>\\{loop, verb} -> run2_
  [ convolver verb [ loopBuf loop bangOn ] ]</code></pre>

  ~convolution~
  </section>
`}})()()(W()(K(f))({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}})(p))(p)(FR)({convolution:L(rt(ht(r)(t)(function(n){return Ut(HE)(_(Ai)(function(a){return function(u){return{loop:a,verb:u}}})(Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")))(Et(n)("https://cdn.jsdelivr.net/gh/andibrae/Reverb.js/Library/StMarysAbbeyReconstructionPhase3.m4a"))})(function(n){return function(a){return Dt(n)([dE(PC)(a.verb)([le(Ce)(a.loop)(_t())])])}})))})}}};var MR=function(){return d.value}(),CS=function(t){return function(e){return function(r){return pe({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(p)(W()(K(f))({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}})(p))(d.value)(MR)({delay:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return Dt(n)([Fa(jn(Ha)(a)(_t()))(function(u){return function(i){return Ot(ft)(.2)([yo(tn)(.03)([u]),yo(tn)(.1)([u]),yo(tn)(.3)([u]),yo(tn)(.7)([u])])}})])}})))})}}};var PR=function(){return d.value}(),ES=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
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
`}})()()(W()(K(f))({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}})(p))(p)(PR)({gain:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return Dt(n)([Ot(ft)(.1)([le(Ce)(a)(_t())])])}})))})}}};var IR=function(){return d.value}(),SS=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
  <h2 id="highpass">Highpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highpass filter</a> lets higher frequencies pass and amortizes lower ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ highpass_ 2000.0
      [ loopBuf buf bangOn ]
  ]
</code></pre>

  ~highpass~
  </section>
`}})()()(W()(K(f))({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}})(p))(p)(IR)({highpass:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([Wc(tu)(2e3)([le(Ce)(a)(_t())])])}})))})}}};var NR=function(){return d.value}(),hS=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
  <h2 id="highshelf">Highshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highshelf filter</a> boosts or attenuates high frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
  [ highshelf_ { frequency: 2000.0, gain: 0. }
      [ loopBuf buf bangOn ]
  ]</code></pre>

  ~highshelf~
  </section>
`}})()()(W()(K(f))({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}})(p))(p)(NR)({highshelf:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([EE(cE(Ct(gt()(J(J(kt)(BC)()()()({reflectSymbol:function(){return"gain"}}))(GC)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:2e3,gain:.4})([le(Ce)(a)(_t())])])}})))})}}};var WR=function(){return d.value}(),TS=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
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
`}})()()(W()(K(f))({reflectType:function(){return"iirFilterEx"}})({reflectSymbol:function(){return"iirFilterEx"}})(p))(p)(WR)({iirFilterEx:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([bE()()(wC(mi)(mi))(new at(Tf()()(20298e-8)(Tf()()(.0004059599)(Tf()()(20298e-8)(Dv))),Tf()()(1.0126964558)(Tf()()(-1.9991880801)(Tf()()(.9873035442)(Dv)))))([le(Ce)(a)(_t())])])}})))})}}};var GR=function(){return d.value}(),xS=function(t){return function(e){return function(r){return pe({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(p)(W()(K(f))({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}})(p))(d.value)(GR)({loopBuf:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/100/100981_1234256-lq.mp3")})(function(n){return function(a){return Dt(n)([le(Wf(Ct(gt()(J(J(J(J(kt)(Rc)()()()({reflectSymbol:function(){return"playbackRate"}}))(h_)()()()({reflectSymbol:function(){return"loopStart"}}))(S_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Lf)()()()({reflectSymbol:function(){return"buffer"}})))(At()())))({buffer:a,playbackRate:.5,loopStart:.1,loopEnd:.6})(_t()),le(Wf(Ct(gt()(J(J(J(J(kt)(Rc)()()()({reflectSymbol:function(){return"playbackRate"}}))(h_)()()()({reflectSymbol:function(){return"loopStart"}}))(S_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Lf)()()()({reflectSymbol:function(){return"buffer"}})))(At()())))({buffer:a,playbackRate:1,loopStart:.5,loopEnd:1.2})(_t()),le(Wf(Ct(gt()(J(J(kt)(Rc)()()()({reflectSymbol:function(){return"playbackRate"}}))(Lf)()()()({reflectSymbol:function(){return"buffer"}})))(At()())))({buffer:a,playbackRate:1.7})(_t())])}})))})}}};var qR=function(){return d.value}(),FS=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
  <h2 id="lowpass">Lowpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowpass filter</a> lets lower frequencies pass and amortizes higher ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ lowpass_ 215.0 [ loopBuf buf bangOn ] ]
</code></pre>

  ~lowpass~
  </section>
`}})()()(W()(K(f))({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}})(p))(p)(qR)({lowpass:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([Bc(Ts)(215)([le(Ce)(a)(_t())])])}})))})}}};var zR=function(){return d.value}(),$S=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
  <h2 id="lowshelf">Lowshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowshelf filter</a> boosts or attenuates lower frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
   [ lowshelf_ { frequency: 91.0, gain: 10.0 }
       [ loopBuf buf bangOn ]
   ]
</code></pre>

  ~lowshelf~
  </section>
`}})()()(W()(K(f))({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}})(p))(p)(zR)({lowshelf:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([hE(fE(Ct(gt()(J(J(kt)(NC)()()()({reflectSymbol:function(){return"gain"}}))(LC)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:91,gain:.4})([le(Ce)(a)(_t())])])}})))})}}};var JR=function(){return d.value}(),MS=function(t){return function(e){return function(r){return pe({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(p)(W()(K(f))({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}})(p))(d.value)(JR)({microphone:L(rt(ht(r)(t)(function(n){return Bs(!0)(!1)})(function(n){return function(a){return Dt(n)([function(){if(a.microphone instanceof B)return eu(function(u){return Ot(ft)(1)([F_(C_)(a.microphone.value0),yo(tn)(.1)([Ot(ft)(.2)([u])])])});if(a.microphone instanceof V)return Ot(ft)(.02)([kE(rf)(440)]);throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Microphone (line 43, column 15 - line 48, column 56): "+[a.microphone.constructor.name])}()])}})))})}}};var XR=function(){return d.value}(),wS=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
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
`}})()()(W()(K(f))({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}})(p))(p)(XR)({notch:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([Gc(Lc(Ct(gt()(J(J(kt)(Oc)()()()({reflectSymbol:function(){return"q"}}))(Ic)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:400,q:1})(T(pr)(Gc(Lc(Ct(gt()(J(J(kt)(Oc)()()()({reflectSymbol:function(){return"q"}}))(Ic)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:880,q:5})(T(pr)(Gc(Lc(Ct(gt()(J(J(kt)(Oc)()()()({reflectSymbol:function(){return"q"}}))(Ic)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:1200,q:10})(T(pr)(Gc(Lc(Ct(gt()(J(J(kt)(Oc)()()()({reflectSymbol:function(){return"q"}}))(Ic)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:2e3,q:20})(T(pr)(Gc(Lc(Ct(gt()(J(J(kt)(Oc)()()()({reflectSymbol:function(){return"q"}}))(Ic)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:3e3,q:30})(T(pr)(le(Ce)(a)(_t())))))))))))])}})))})}}};var KR=function(){return d.value}(),PS=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
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
`}})()()(W()(K(f))({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}})(p))(p)(KR)({peaking:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([Uc(Nc(Ct(gt()(J(J(J(kt)(Mc)()()()({reflectSymbol:function(){return"q"}}))(wc)()()()({reflectSymbol:function(){return"gain"}}))(Pc)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:400,q:1,gain:-20})(T(pr)(Uc(Nc(Ct(gt()(J(J(J(kt)(Mc)()()()({reflectSymbol:function(){return"q"}}))(wc)()()()({reflectSymbol:function(){return"gain"}}))(Pc)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:880,q:5,gain:20})(T(pr)(Uc(Nc(Ct(gt()(J(J(J(kt)(Mc)()()()({reflectSymbol:function(){return"q"}}))(wc)()()()({reflectSymbol:function(){return"gain"}}))(Pc)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:1200,q:10,gain:-20})(T(pr)(Uc(Nc(Ct(gt()(J(J(J(kt)(Mc)()()()({reflectSymbol:function(){return"q"}}))(wc)()()()({reflectSymbol:function(){return"gain"}}))(Pc)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:2e3,q:20,gain:20})(T(pr)(Uc(Nc(Ct(gt()(J(J(J(kt)(Mc)()()()({reflectSymbol:function(){return"q"}}))(wc)()()()({reflectSymbol:function(){return"gain"}}))(Pc)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:3e3,q:30,gain:-20})(T(pr)(le(Ce)(a)(_t())))))))))))])}})))})}}};var ZR=function(){return d.value}(),OS=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
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
`}})()()(W()(K(f))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(p))(p)(ZR)({periodic:L(rt(ht(r)(t)(function(n){return T(ya)(void 0)})(function(n){return function(a){return Dt(n)([Ot(ft)(.2)([yi(bi(Ct(gt()(J(J(kt)(di(vi(aa)))()()()({reflectSymbol:function(){return"spec"}}))(Di)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:140,spec:new at(Pe(we(aa)()(vn)()(qa))(.1)(Pe(we(gu)()(Dn)()(vn))(.2)(Pe(we(Cu)()(dn)()(Dn))(.3)(Pe(we(Eu)()(Su)()(dn))(.4)(Gu)))),Pe(we(aa)()(vn)()(qa))(.4)(Pe(we(gu)()(Dn)()(vn))(.3)(Pe(we(Cu)()(dn)()(Dn))(.2)(Pe(we(Eu)()(Su)()(dn))(.1)(Gu)))))})(_t())])])}})))})}}};var eN=function(){return d.value}(),IS=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
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
`}})()()(W()(K(f))({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}})(p))(p)(eN)({playBuf:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/470/470035_9564355-lq.mp3")})(function(n){return function(a){return Dt(n)([jn(T_(Ct(gt()(J(J(J(kt)(IC)()()()({reflectSymbol:function(){return"duration"}}))(OC)()()()({reflectSymbol:function(){return"bufferOffset"}}))(E_)()()()({reflectSymbol:function(){return"buffer"}})))(At()())))({buffer:a,duration:3,bufferOffset:4.2})(_t())])}})))})}}};var Db=function(){function t(){}return t.value=new t,t}();var RS={attr:function(t){return function(e){return b({key:"controls",value:U(e)})}}};var db=function(){function t(){}return t.value=new t,t}();var NS={attr:function(t){return function(e){return b({key:"src",value:U(e)})}}};var bb=function(t){return function(e){return function(r){return new O(j(t)("audio")(e)(G(r)))}}};var oN=function(t){return function(e){return function(r){return function(n){return VD(t)(n)(F_(e)(r))}}}},iN=function(){return d.value}(),LS=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
  <h2 id="recorder">Recorder</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioDestinationNode">recorder</a> allows you to record audio. It takes a callback that you can use to stash the recorded audio somewhere, like in a file for example, as the example below does. You can use it as a simple note-taking app \u{1F399}\uFE0F.</p>

  <pre><code>\\cb m -> recorder cb (microphone m)</code></pre>

  ~recorder~
  </section>
`}})()()(W()(K(f))({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}})(p))(p)(iN)({recorder:L(rt(Gr(k)(function(n){return function(a){var u=Ll(bu(f))(tt(et))(a),i=Ll(bu(f))(tt(et))(function(l){return l.left}(u)),o=function(l){return l.right}(i),m=Jc(I(f))(h(k))(r)(function(l){return l.right}(u)),s=function(l){return l.left}(i);return Ue(p)([gn(p)(N(I(f))(Y(h(k))(Z(Dc)(Vt.value)("cursor: pointer;")))(_(g)(function(l){return Z(dr)(mr.value)(Ye(x(function(){if(l.e instanceof zo)return T(f)(void 0);if(l.e instanceof Vo)return X(nt)(X(nt)(X(nt)(l.e.value0)(t(T(f)(void 0))))(_a(f)(Ke)(l.rec)(function(){var v=i_(Vv);return function(D){return v(Ls(D))}}())))(n(Xt.create(Tu.value)));if(l.e instanceof Tu)return function(){l.cncl();var D=x_();n(new Xt(zo.value))();var c=ko(q(Lr)(_(Ai)(function(C){return C.microphone})(Bs(!0)(!1)))(function(C){return br(Nr)(function(){var mt=Yt(T(f)(T(f)(void 0)))(function(ve){return function(){var Ze=ua(fr)(),lr=nf(Ze)(),pt=Bf([oN(TD)(C_)(ve)(function(he){return function(){return n(new jt(new Xt(he)))(),Ae(F)(Fs(he)(D))(),nS("audio/ogg; codecs=opus")(function(Hr){return n(jt.create(jt.create(Hr)))})(he)()}})])(Hf),De=$t(pt)(function(he){return he(lr)})();return function(){De(),q(hn)(vE(D))(vr(f)(Ke)(function(){var Hr=i_(Vv);return function(So){return Hr(Ls(So))}}()))();var ae=q_(fr)(Ze)();return Rn(f)(ae!=="closed")(Cn(fr)(Ze))()}}})(C)();return n(new Xt(new Vo(mt)))(),mt})}))();return t(function(){return n(Xt.create(Tu.value))(),Ho(gi(c))()})(),void 0};throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Recorder (line 63, column 47 - line 106, column 52): "+[l.e.constructor.name])}())))})($n(h(k))(N(I(f))(Y(h(k))(V.value))(_(g)(B.create)(o)))(_(g)(ml)($n(h(k))(N(I(f))(Y(h(k))(T(f)(void 0)))(_(g)(function(l){return l.value0})(r)))(_(g)(function(l){return function(v){return function(D){return{e:l,cncl:v,rec:D}}}})(m)))))))([un(ue)(_(g)(function(l){if(l instanceof Tu)return"Turn on";if(l instanceof zo)return"Loading...";if(l instanceof Vo)return"Turn off";throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Recorder (line 117, column 31 - line 120, column 56): "+[l.constructor.name])})(m))]),Ue(p)([bb(p)(N(I(f))(Y(h(k))(Z(RS)(Db.value)("true")))(N(I(f))(Y(h(k))(Z($v)(Vt.value)("display:none;")))(N(I(f))(_(g)(function(l){return Z(NS)(db.value)(l)})(s))(_(g)(x(Z($v)(Vt.value)("display:block;")))(s)))))([])])])}})))})}}};var cN=function(){return d.value}(),WS=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sawtoothOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(W()(K(f))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(p))(p)(cN)({periodic:L(rt(ht(r)(t)(function(n){return T(ya)(void 0)})(function(n){return function(a){return Dt(n)([Ot(ft)(.2)([AE(MC)(448)(_t())])])}})))})}}};var _N=function(){return d.value}(),BS=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
  <h2 id="sine">Sine wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sine wave oscillator</a> plays back a sine wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sinOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(W()(K(f))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(p))(p)(_N)({periodic:L(rt(ht(r)(t)(function(n){return T(ya)(void 0)})(function(n){return function(a){return Dt(n)([Ot(ft)(.2)([Gf(rf)(448)(_t())])])}})))})}}};var sN=function(){return d.value}(),GS=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
  <h2 id="sawtooth">Square wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ squareOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(W()(K(f))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(p))(p)(sN)({periodic:L(rt(ht(r)(t)(function(n){return T(ya)(void 0)})(function(n){return function(a){return Dt(n)([Ot(ft)(.2)([$_($c)(448)(_t())])])}})))})}}};var vN=function(){return d.value}(),US=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/StereoPannerNode">stereo panner</a> pans audio in the stereo plane. <code>-1.0</code> represents hard left, and <code>1.0</code> represents hard right, as in the example below.</p>

  <pre><code>\\buf -> run2_
  [ pan_ 1.0 [ loopBuf buf bangOn ] ]</code></pre>

  ~pan~
  </section>
`}})()()(W()(K(f))({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}})(p))(p)(vN)({pan:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return Dt(n)([TE($C)(1)([le(Ce)(a)(_t())])])}})))})}}};var dN=function(){return d.value}(),qS=wt({reflectType:function(){return`<ul>
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
`}})()()(K(f))(p)(dN)({});var yN=function(){return d.value}(),HS=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ triangleOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(W()(K(f))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(p))(p)(yN)({periodic:L(rt(ht(r)(t)(function(n){return T(ya)(void 0)})(function(n){return function(a){return Dt(n)([Ot(ft)(.2)([Os(Es)(448)(_t())])])}})))})}}};var kN=function(){return d.value}(),zS=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
  <h2 id="waveshaper">Waveshaper</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/WaveshaperNode">waveshaper node</a>, aka distortion, uses a <a href="https://en.wikipedia.org/wiki/Waveshaper">waveshaping function</a> to add warmth to a sound.</p>

  <pre><code>~code~</code></pre>

  ~waveShaper~
  </section>
`}})()()(W()(W()(K(f))({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}})(p))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}})(p))(p)(kN)({code:L(or(ue)(`do
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
    [ waveShaper (makeFloatArray (makeDistortionCurve 400.0)) [ loopBuf buf bangOn ] ]`)),waveShaper:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){var u=function(i){var o=Zi/180;return _($e)(function(m){var s=ze(m)*2/ze(44100)-1;return(3+i)*s*20*o/(Zi+i*sm(Ia)(mf)(s))})(pn(0)(44099))};return Dt(n)([xE(iE)(eb(u(400)))([le(Ce)(a)(_t())])])}})))})}}};var CN=function(){return d.value}(),VS=function(t){return function(e){return function(r){return function(n){var a=X(nt)(e(qf.value))(ln),u=wa(t)(r);return wt({reflectType:function(){return`<div>
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
</div>`}})()()(W()(W()(W()(W()(W()(W()(W()(W()(W()(W()(W()(W()(W()(W()(W()(W()(on()(W()(W()(W()(W()(W()(W()(W()(W()(W()(W()(W()(W()(K(f))({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}})(p))({reflectType:function(){return"triangleOsc"}})({reflectSymbol:function(){return"triangleOsc"}})(p))({reflectType:function(){return"toc"}})({reflectSymbol:function(){return"toc"}})(p))({reflectType:function(){return"squareOsc"}})({reflectSymbol:function(){return"squareOsc"}})(p))({reflectType:function(){return"sinOsc"}})({reflectSymbol:function(){return"sinOsc"}})(p))({reflectType:function(){return"sawtoothOsc"}})({reflectSymbol:function(){return"sawtoothOsc"}})(p))({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}})(p))({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}})(p))({reflectType:function(){return"periodicOsc"}})({reflectSymbol:function(){return"periodicOsc"}})(p))({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}})(p))({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}})(p))({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}})(p))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(p))({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}})(p))({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}})(p))({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}})(p))({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}})(p))({reflectType:function(){return"iirFilter"}})({reflectSymbol:function(){return"iirFilter"}})(p))({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}})(p))({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}})(p))({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}})(p))({reflectType:function(){return"drumroll"}})({reflectSymbol:function(){return"drumroll"}})(p))({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}})(p))({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}})(p))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}})(p))({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}})(p))({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}})(p))({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}})(p))({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}})(p))(p)(CN)({drumroll:L(rt(Us("\u{1F941}")(n)(u)(function(i){return Et(i)("https://freesound.org/data/previews/50/50711_179538-lq.mp3")})(function(i){return function(o){return Dt(i)([Ot(ft)(1)([le(Ce)(o)(_t())])])}}))),toc:L(qS),allpass:L(aS(u)(e)(n)),analyser:L(vS(u)(e)(n)),bandpass:L(DS(u)(e)(n)),constant:L(kS(u)(e)(n)),compression:L(dS(u)(e)(n)),convolution:L(gS(u)(e)(n)),delay:L(CS(u)(e)(n)),gain:L(ES(u)(e)(n)),highpass:L(SS(u)(e)(n)),highshelf:L(hS(u)(e)(n)),iirFilter:L(TS(u)(e)(n)),loopBuf:L(xS(u)(e)(n)),lowshelf:L($S(u)(e)(n)),lowpass:L(FS(u)(e)(n)),notch:L(wS(u)(e)(n)),playBuf:L(IS(u)(e)(n)),peaking:L(PS(u)(e)(n)),microphone:L(MS(u)(e)(n)),pan:L(US(u)(e)(n)),periodicOsc:L(OS(u)(e)(n)),recorder:L(LS(u)(e)(n)),sawtoothOsc:L(WS(u)(e)(n)),sinOsc:L(BS(u)(e)(n)),squareOsc:L(GS(u)(e)(n)),triangleOsc:L(HS(u)(e)(n)),waveShaper:L(zS(u)(e)(n)),next:Ma(I(f))(h(k))(n)(a)})}}}};var yb=function(){function t(){}return t.value=new t,t}(),JS={attr:function(t){return function(e){return b({key:"checked",value:U(e)})}}};var Co=function(){function t(){}return t.value=new t,t}();var Jo={attr:function(t){return function(e){return b({key:"type",value:U(e)})}}};var Eo=function(t){return function(e){return function(r){return new O(j(t)("input")(e)(G(r)))}}};var TN=function(t){return t},Ks=function(t){return function(e){return function(r){return uo(t)(N(t.Plus0().Alt0())(Y(t)(e))(r))}}};var V_=function(t){return function(e){return t(e)}},uf=function(t){return{map:function(e){return function(r){return function(n){return r(_(t)(function(a){return function(u){return a(e(u))}})(n))}}}}},Si=function(t){return function(e){return function(r){return function(n){return V_(_(uf(t.Filterable1().Functor1()))(e)(r))(_(t.Filterable1().Functor1())(Qf)(n))}}}};var tl=function(t){return Si(t)(x)};var iu=TN;var jS=function(t){return function(e){return iu(function(r){return $r(h(k))(N(I(f))(Y(h(k))(V_(t)(r)))(_(g)(function(n){return V_(n)(r)})(e)))})}},Ab=function(t){return{apply:function(e){return function(r){return function(n){return r(e(_(t)(wu(Yo))(n)))}}},Functor0:function(){return uf(t)}}};var el=function(t){return function(e){return Wt(function(r){return $t(e)(function(n){return function(){var u=U_(t)();return r({acTime:u,value:n})()}})})}};var XS=function(t){return function(e){return function(r){var n=function(a){return function(u){return function(i){return function(o){return function(m){return function(s){return function(){var v=Rr(i)();return Rn(f)(v)(function(){var c=U_(t)(),C=xp(zk(Mu(Ia)(u-c-.04)(.01)*1e3))(function(){var mt=Rr(i)();return Rn(f)(mt)(function(){return Tn(u)(m)(),a(u)(),n(a)(u+s)(i)(o)(m)(s)()})()})();return Tn(new B(C))(o)()})()}}}}}}};return Wt(function(a){return function(){var i=sr(!0)(),o=sr(V.value)(),m=U_(t)(),s=sr(m+e)();n(a)(e)(i)(o)(s)(e)();var l=$t(r)(function(v){return function(){q(hn)(Rr(o))(vr(f)(Ke)(Gl))();var c=Rr(s)();return n(a)(c+v)(i)(o)(s)(v)()}})();return X(nt)(X(nt)(l)(Tn(!1)(i)))(q(hn)(Rr(o))(vr(f)(Ke)(Gl)))}})}}};var Pa=function(t){return function(e){return function(r){return function(n){return function(a){var u=r===t||n===e;if(u)return e;var i=(n-e)/(r-t),o=e-i*t;return i*a+o}}}}};var xN=function(){return d.value}(),QS=function(t){return function(e){return function(r){return function(n){return wt({reflectType:function(){return`<section>
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

</section>`}})()()(W()(W()(K(f))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(p))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}})(p))(p)(xN)({txt:L(or(ue)(`module Main where

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
  )`)),empl:L(rt(Ya()(k)(Nu({reflectSymbol:function(){return"cbx"}})()()()(ur({reflectSymbol:function(){return"cbx0"}})()()(ur({reflectSymbol:function(){return"cbx1"}})()()(ur({reflectSymbol:function(){return"cbx2"}})()()(ur({reflectSymbol:function(){return"cbx3"}})()()(Gn)()()()())()()()())()()()())()()()())(Nu({reflectSymbol:function(){return"startStop"}})()()()(ur({reflectSymbol:function(){return"start"}})()()(ur({reflectSymbol:function(){return"stop"}})()()(Gn)()()()())()()()())(Gn)()()()())()()()())(d.value)(function(a){return function(u){var i=N(I(f))(Y(h(k))(void 0))(u.startStop.start),o=function(D){return Ks(h(k))(!1)(Iu(h(k))(x(Du(La)))(D)(!1))},m=o(u.cbx.cbx3),s=o(u.cbx.cbx2),l=o(u.cbx.cbx1),v=o(u.cbx.cbx0);return Ue(p)([gn(p)(sn(zt)(S(f))(_(g)(function(){var D=Z(dr)(mr.value);return function(c){return D(Ye(x(c)))}}()))([Qe(g)($n(h(k))(N(I(f))(Y(h(k))(T(f)(void 0)))(_(g)(function(D){return D.value0})(n)))(Q(g)(i)(tt(et))))(function(D){return function(){D();var C=ua(fr)(),ut=Hu(fr)(C)(),mt=function(Ze){return function(lr){return function(pt){return Ul(h(k))(function(De){return function(he){var ae=he.value1+(De.value1-he.value0)*function(){return De.value0?Ze:1}();return new at(new at(De.value1,ae),ae)}})(Si(h(k))(at.create)(lr)(pt))(new at(0,0))}}},ve=jc(C)(ji(k)(_(g)(function(){var Ze=We(ga)(.04);return function(lr){return Ze(function(pt){return pt.acTime}(lr))}}())(el(C)(af)))(function(Ze){var lr=function(Hr){return function(So){return uo(h(k))(Ze)(_(g)(ml)(uo(h(k))(So)(_(g)(function(zu){return function(Mi){return function(cu){return{f:zu,a:Mi,t:cu}}}})(Hr))))}},pt=_(g)(function(Hr){return Hr?4:1})(tl(h(k))(m)(Ze)),De=mt(4)(s)(Ze),he=_(g)(function(Hr){return Hr?4:1})(tl(h(k))(l)(Ze)),ae=mt(8)(v)(Ze);return[qr(ft)(0)(Qe(g)(lr(ae)(he))(function(Hr){return En()(Wr)({n:Pa(1)(.01)(4)(.15)(Hr.a)*Vp(Zi*Hr.f)+.15,o:Hr.t,t:Go})}))([yi(bi(Ct(gt()(J(J(kt)(di(vi(aa)))()()()({reflectSymbol:function(){return"spec"}}))(Di)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:325.6,spec:new at(Pe(we(aa)()(vn)()(qa))(.3)(Pe(we(gu)()(Dn)()(vn))(-.1)(Pe(we(Cu)()(dn)()(Dn))(.7)(Pe(we(Eu)()(Su)()(dn))(-.4)(Gu)))),Pe(we(aa)()(vn)()(qa))(.6)(Pe(we(gu)()(Dn)()(vn))(.3)(Pe(we(Cu)()(dn)()(Dn))(.2)(Pe(we(Eu)()(Su)()(dn))(0)(Gu)))))})(Sr(zt)(S(f))([_t(),Qe(g)(lr(De)(pt))(function(Hr){return go()(Wr)({n:325.6+Pa(1)(3)(4)(15.5)(Hr.a)*Vp(Zi*Hr.f),o:Hr.t,t:Go})})]))])]}))(),ne=X(nt)(X(nt)(ve)(ut))(Cn(fr)(C));return t(X(nt)(ne)(a.startStop.start(void 0)))(),a.startStop.stop(ne)()}}),Qe(g)(u.startStop.stop)(function(D){return X(nt)(D)(X(nt)(t(T(f)(void 0)))(a.startStop.start(void 0)))})]))([un(ue)(Sr(zt)(S(f))([Q(g)(i)("Turn on"),Q(g)(u.startStop.stop)("Turn off")]))]),Te(p)(sn(zt)(S(f))(_(g)(Z(st)(Vt.value)))([Q(g)(u.startStop.stop)("display:block;"),Q(g)(i)("display:none;")]))(_($e)(function(D){return Eo(p)(Sr(zt)(S(f))([Y(h(k))(Z(Jo)(Co.value)("checkbox")),Y(h(k))(Z(dr)(mr.value)(Ye(x(D(void 0))))),Q(g)(i)(Z(JS)(yb.value)("false"))]))([])})(mm($e)([function(D){return D.cbx0},function(D){return D.cbx1},function(D){return D.cbx2},function(D){return D.cbx3}])(a.cbx)))])}})))})}}}};var kb={recip:function(t){return 1/t},Ring0:function(){return mf}};var gb=function(t){return function(e){return{EuclideanRing0:function(){return t},DivisionRing1:function(){return e}}}};function rl(t){return function(){return function(e){return t(e)()}}}function nl(t){return function(e){return function(r){return function(n){return function(){return n.addEventListener(t,e,r)}}}}}function al(t){return function(e){return function(r){return function(n){return function(){return n.removeEventListener(t,e,r)}}}}}function Cb(t){return t.clientX}function Eb(t){return t.clientY}function J_(t){return t.button}var j_=It("MouseEvent");var KS=function(t){return function(e){return Wt(function(r){return $t(e)(function(n){return function(){var u=Rr(t.buttons)();return r({value:n,buttons:u})()}})})}};var YS=function(){var e=sr(V.value)(),r=sr(uv)(),n=_(F)(vb)(Ei)(),a=rl(function(m){return vr(f)(Ke)(function(s){return Tn(new B({x:Cb(s),y:Eb(s)}))(e)})(j_(m))})(),u=rl(function(m){return vr(f)(Ke)(function(s){return tc(GA(Xr)(J_(s)))(r)})(j_(m))})(),i=rl(function(m){return vr(f)(Ke)(function(s){return tc(Tp(Xr)(J_(s)))(r)})(j_(m))})();nl(nn()("mousemove"))(a)(!1)(n)(),nl(nn()("mousedown"))(u)(!1)(n)(),nl(nn()("mouseup"))(i)(!1)(n)();var o=function(){return al(nn()("mousemove"))(a)(!1)(n)(),al(nn()("mousedown"))(u)(!1)(n)(),al(nn()("mouseup"))(i)(!1)(n)()};return{position:e,buttons:r,dispose:o}},ZS=Wt(function(t){return function(){var r=_(F)(vb)(Ei)(),n=rl(function(a){return vr(f)(Ke)(function(u){return t(J_(u))})(j_(a))})();return nl(nn()("mousedown"))(n)(!1)(r)(),al(nn()("mousedown"))(n)(!1)(r)}});var e0=function(t){return iu(function(e){return _(g)(function(r){return r.value(r.buttons)})(KS(t)(e))})};var Tb=function(t){return t};function tm(){return Date.now()}var h0=function(t){return Wt(function(e){return $t(t)(function(r){return function(){var a=tm();return e({time:a,value:r})()}})})};var l1=iu(function(t){return _(g)(function(e){return e.value(e.time)})(h0(t))}),Fb=_(uf(g))(function(){var t=O_(UE);return function(e){return t(Tb(e))}}())(l1);var p1=function(t){var e=function(u){return function(i){return function(o){return function(m){return function(s){return function(l){return function(v){var D=We(i.DivisionRing1().Ring0().Semiring0())(Ca(i.DivisionRing1().Ring0().Semiring0()))(Ca(i.DivisionRing1().Ring0().Semiring0())),c=function(C){return function(ut){if(C.last instanceof V)return ut;if(C.last instanceof B)return We(o)(ut)(m(function(mt){return Ku(i.EuclideanRing0())(In(i.DivisionRing1().Ring0().Semiring0())(mt(We(o)(C.last.value0.value1)(C.now.value1)))($u(i.DivisionRing1().Ring0())(C.now.value0)(C.last.value0.value0)))(D)}));throw new Error("Failed pattern match at WAGS.Example.Docs.FixEx (line 102, column 5 - line 102, column 35): "+[C.constructor.name,ut.constructor.name])}};return iu(function(C){var ut=V_(v)(Q(u.Filterable1().Functor1())(C)(tt(et))),mt=$p(u)(Si(u)(at.create)(l)(ut)),ve=Iu(u)(c)(mt)(s);return uo(u)(ve)(C)})}}}}}}},r=function(u){return function(i){return e(u)(i)(i.DivisionRing1().Ring0().Semiring0())(function(o){return o(tt(et))})}},n=function(u){return function(i){return iu(function(o){return ql(h(k))(function(m){var s=i(Ks(h(k))(u)(m));return{input:tl(h(k))(s)(o),output:uo(h(k))(m)(o)}})})}},a=function(u){return function(i){return function(o){if(BA(u))return-8*(i-1)-o*2;if(nr)return 2*(4-i);throw new Error("Failed pattern match at WAGS.Example.Docs.FixEx (line 62, column 3 - line 64, column 34): "+[u.constructor.name,i.constructor.name,o.constructor.name])}}};return n(2)(function(u){return r(h(k))(gb(yl)(kb))(2)(_(uf(g))(Cr())(Fb))(function(){var i=n(10)(function(o){return r(h(k))(gb(yl)(kb))(10)(_(uf(g))(Cr())(Fb))(Ut(Ab(g))(Ut(Ab(g))(_(uf(g))(a)(e0(t)))(u))(o))});return jS(i)(Q(g)(ZS)(i))}())})},s1=function(){return d.value}(),T0=function(t){return function(e){return function(r){return function(n){return wt({reflectType:function(){return`<section>
  <h2>Fix</h2>

  <p>Fix, like it's equivalent in wags that we've already seen, creates a feedback loop. However, in this case, we are talking about a feedback loop of <i>events</i>, not sound.</p>

  <p>At first glance, it may not be clear why we need an event stream to feed back into itself? It seems prone to saturation: if you have a counter that feeds back into itself with a delay, after a few seconds you'll have so many events that it will crash your browser (I've tried it!).</p>

  <p>However, there's one important circumstance where you need fixed points: when an event can only be defined in terms of itself. One classic category of this is the <i>differential equation</i>. Differential equations allow you to produce <a href="https://en.wikipedia.org/wiki/Simple_harmonic_motion">Slinky effects, aka simple harmonic motion,</a> and a lot of other neat behaviors that are difficult to produce via other means.</p>

  <p>Let's listen to the sound of simple harmonic motion in the example below, courtesy of <code>fix</code>. The differential equation in the example below comes from Phil Freeman, the creator of the PureScript language and the author of the <code>purescript-behaviors</code> package. When you click "Turn on", you won't hear much, but press and release your mouse anywhere on the screen to hear the differential equation take flight!</p>

  <pre><code>~txt~</code></pre>

  ~empl~

  <p>When working with stateful events, a good way to decide if you should use <code>fold</code> versus <code>fix</code> is to ask the following question: can I incrementally change my state based on an initial state, or is my state defined in terms of how it changes? If you can incrementally change your state, go with <code>fold</code>. If, on the other hand, your state is defined in terms of how it changes, go with <code>fix</code>.</p>
</section>`}})()()(W()(W()(K(f))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(p))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}})(p))(p)(s1)({txt:L(or(ue)(`module Main

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
  )`)),empl:L(rt(Ya()(k)(ur({reflectSymbol:function(){return"start"}})()()(ur({reflectSymbol:function(){return"stop"}})()()(Gn)()()()())()()()())(d.value)(function(a){return function(u){var i=N(I(f))(Y(h(k))(void 0))(u.start);return Ue(p)([gn(p)(sn(zt)(S(f))(_(g)(function(){var o=Z(dr)(mr.value);return function(m){return o(Ye(x(m)))}}()))([Qe(g)($n(h(k))(N(I(f))(Y(h(k))(T(f)(void 0)))(_(g)(function(o){return o.value0})(n)))(Q(g)(i)(tt(et))))(function(o){return function(){o();var s=ua(fr)(),l=Hu(fr)(s)(),v=YS(),D=r_(0)(1e4)(),c=function(pt){return{o:pt.value0+.04,n:pt.value1,t:Go}},C=_(co)(function(pt){return pt-.5})(m_(Ag)),ut=q(wf)(C)(function(pt){return q(wf)(C)(function(De){return q(wf)(C)(function(he){return q(wf)(C)(function(ae){return T(p_)(Pe(we(aa)()(vn)()(qa))(pt)(Pe(we(gu)()(Dn)()(vn))(De)(Pe(we(Cu)()(dn)()(Dn))(he)(Pe(we(Eu)()(Su)()(dn))(ae)(Gu)))))})})})}),mt=Ut(Pf)(_(co)(at.create)(ut))(ut),ve=Ut(Pf)(Ut(Pf)(Ut(Pf)(_(co)(function(pt){return function(De){return function(he){return function(ae){return{s0:pt,s1:De,s2:he,s3:ae}}}}})(mt))(mt))(mt))(mt),ne=Sc(ve)({newSeed:kc(D),size:5}),Ze=jc(s)(ji(k)(_(g)(function(pt){return new at(pt.acTime,pt.value)})(el(s)(tl(h(k))(p1(v))(af))))(function(pt){return[qr(ft)(0)(_(g)(function(){var De=En()(Wr),he=Vn(bn)(function(ae){return Mu(Ia)(-.4)(.5*(ae-1))});return function(ae){return De(c(he(ae)))}}())(pt))([Bc(BD(Ct(gt()(J(J(kt)(WC)()()()({reflectSymbol:function(){return"q"}}))(xD)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:90.4,q:20})([gE($c)(90.4)])]),qr(ft)(0)(_(g)(function(){var De=En()(Wr),he=Vn(bn)(function(ae){return Mu(Ia)(-.2)(.4*(ae-3))});return function(ae){return De(c(he(ae)))}}())(pt))([kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:90.4*4,q:20})([yi(bi(Ct(gt()(J(J(kt)(di(vi(aa)))()()()({reflectSymbol:function(){return"spec"}}))(Di)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:90.4*3.02,spec:ne.s0})(N(I(f))(_t())(_(g)(function(){var De=go()(Wr),he=Vn(bn)(function(ae){return 90.4*3.02+14*(ae-1)});return function(ae){return De(c(he(ae)))}}())(pt)))])]),qr(ft)(0)(_(g)(function(){var De=En()(Wr),he=Vn(bn)(function(ae){return Mu(Ia)(-.1)(.2*(ae-6))});return function(ae){return De(c(he(ae)))}}())(pt))([kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:90.4*6,q:20})([yi(bi(Ct(gt()(J(J(kt)(di(vi(aa)))()()()({reflectSymbol:function(){return"spec"}}))(Di)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:90.4*5.07,spec:ne.s1})(N(I(f))(_t())(_(g)(function(){var De=go()(Wr),he=Vn(bn)(function(ae){return 90.4*5.07+18*(ae-1)});return function(ae){return De(c(he(ae)))}}())(pt)))])]),qr(ft)(0)(_(g)(function(){var De=En()(Wr),he=Vn(bn)(function(ae){return Mu(Ia)(0)(.2*(ae-3))});return function(ae){return De(c(he(ae)))}}())(pt))([kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:90.4*8,q:20})([yi(bi(Ct(gt()(J(J(kt)(di(vi(aa)))()()()({reflectSymbol:function(){return"spec"}}))(Di)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:90.4*7.13,spec:ne.s2})(N(I(f))(_t())(_(g)(function(){var De=go()(Wr),he=Vn(bn)(function(ae){return 90.4*7.13+32*(ae-1)});return function(ae){return De(c(he(ae)))}}())(pt)))])]),qr(ft)(0)(_(g)(function(){var De=En()(Wr),he=Vn(bn)(function(ae){return Mu(Ia)(0)(.1*(ae-7))});return function(ae){return De(c(he(ae)))}}())(pt))([yi(bi(Ct(gt()(J(J(kt)(di(vi(aa)))()()()({reflectSymbol:function(){return"spec"}}))(Di)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:90.4*9.14,spec:ne.s3})(N(I(f))(_t())(_(g)(function(){var De=go()(Wr),he=Vn(bn)(function(ae){return 90.4*9.14+31*(ae-1)});return function(ae){return De(c(he(ae)))}}())(pt)))])]}))(),lr=X(nt)(X(nt)(Ze)(l))(Cn(fr)(s));return t(X(nt)(lr)(a.start(void 0)))(),a.stop(lr)()}}),Qe(g)(u.stop)(function(o){return X(nt)(o)(X(nt)(t(T(f)(void 0)))(a.start(void 0)))})]))([un(ue)(Sr(zt)(S(f))([Q(g)(i)("Turn on"),Q(g)(u.stop)("Turn off")]))])])}})))})}}}};var v1=function(){return d.value}(),x0=function(t){return function(e){return function(r){return function(n){var a=wa(t)(r);return wt({reflectType:function(){return`<div>
  <h1>State</h1>

  <h3>Or Events 2.0</h3>
  <p>
    The name of this section is a bit of a nisnomer. While it will address the issue of maintaining state in an audio graph, it's really just about two mechanisms you can use to make an <code>Event</code> stateful. One is called <code>fold</code>, and the other is called <code>fix</code>. Both are part of the <code>IsEvent</code> typeclass, which means you get them for free when working with events.
  </p>

  ~fold~
  ~fix~

  <h2>Next steps</h2>
  <p>Using <code>fold</code> and <code>fix</code>, we can create internal state in our Web Audio works that would be really tedious and error-prone to achieve in vanilla JS or other compile-to-JS languages. There's still one nagging issue that we haven't addressed, though. For all of the flexibility we can achieve with events, we still can't flex the audio graph itself, meaning that we can't add or remove components. In the next section, we'll learn how to do that with <a ~next~ style="cursor:pointer;">subgraphs</a>.</p>
</div>`}})()()(W()(W()(on()(K(f))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(p))({reflectType:function(){return"fold"}})({reflectSymbol:function(){return"fold"}})(p))({reflectType:function(){return"fix"}})({reflectSymbol:function(){return"fix"}})(p))(p)(v1)({next:Ma(I(f))(h(k))(n)(X(nt)(e(B_.value))(ln)),fold:L(QS(a)(e)(r)(n)),fix:L(T0(a)(e)(r)(n))})}}}};var d1=function(){function t(){}return t.value=new t,t}(),F0=function(){function t(){}return t.value=new t,t}(),$b=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),b1=`module Main where

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
`;var y1=function(){return d.value}(),A1=function(t){return function(e){return function(r){return Y(t)(Vf(e)(Rf)({x:hD,o:r}))}}},k1=function(t){return function(e){return function(r){return Y(t)(Vf(e)(Rf)({x:TC,o:r}))}}},g1=Ka(Bn)(ze)(function(t){var e=function(a){return N(I(f))(A1(h(k))()(a+.27*(t*Yi(1.005)(t))))(k1(h(k))()(a+3+.3*(t*Yi(1.005)(t))))},r=function(a){return Y(h(k))(En()(wn)({p:[0,.4,.1,.05,.01,0],o:a+.3*(t*Yi(1.005)(t)),d:.8}))},n=function(a){return function(u){return qr(ft)(0)(r(a))([Gf(rf)(200+t*u)(e(a))])}};return[n(.2)(4),n(.3)(6),n(.45)(14),n(.7)(20)]}),$0=function(t){return function(e){return function(r){return pe({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(p)(W()(W()(K(f))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(p))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}})(p))(d.value)(y1)({txt:L(or(ue)(b1)),ex0:L(rt(Gr(k)(function(n){return Ka(Bn)(function(a){return N(I(f))(Y(h(k))(d1.value))(a)})(function(a){return Ue(p)([gn(p)(Qe(g)($n(h(k))(N(I(f))(Y(h(k))(T(f)(void 0)))(_(g)(function(u){return u.value0})(r)))(_(g)(at.create)(a)))(function(u){return Z(dr)(mr.value)(Ye(x(function(){return u.value0 instanceof $b?X(nt)(X(nt)(u.value0.value0)(n(F0.value)))(t(T(f)(void 0))):function(){u.value1();var o=qs([Ot(ft)(1)(Ja(Zo)(_($e)(g1)(pn(0)(100))))])();return t(X(nt)(o)(n(F0.value)))(),n(new $b(o))()}}())))}))([un(ue)(Qe(g)(a)(function(u){return u instanceof $b?"Turn off":"Turn on"}))])])})})))})}}};var hi=function(){function t(){}return t.value=new t,t}();var ff={attr:function(t){return function(e){return b({key:"max",value:U(e)})}}};var Ti=function(){function t(){}return t.value=new t,t}();var cf={attr:function(t){return function(e){return b({key:"min",value:U(e)})}}};var xi=function(){function t(){}return t.value=new t,t}();var lf={attr:function(t){return function(e){return b({key:"input",value:ct(e)})}}};var Fi=function(){function t(){}return t.value=new t,t}(),_f={attr:function(t){return function(e){return b({key:"step",value:U(e)})}}};var $i=function(){function t(){}return t.value=new t,t}();var pf={attr:function(t){return function(e){return b({key:"value",value:U(e)})}}};var jo=function(t){return function(e){return function(r){return N(t)(e)(r(void 0))}}};var E1=tg,fu={convert:function(t){return t}},X_={convert:function(t){return u_(t)}},w0=function(t){return t},Mb=function(t){return t.convert},Va=function(t){return function(e){return function(r){return bt(E1)(u_(e))(Mb(t)(r(void 0)))}}};var Q_=function(t){return function(e){return function(r){return function(n){return sn(eg)(e)(r)(w0(Mb(t)(n)))}}}};function O0(t){return t.target}var ul=function(t){return Zr(O0(t))};var T1=`module Main where

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
`,x1=function(){return d.value}(),F1="https://freesound.org/data/previews/100/100981_1234256-lq.mp3",I0=function(t){return function(e){return function(r){return pe({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(p)(W()(W()(W()(K(f))({reflectType:function(){return"wagtxt"}})({reflectSymbol:function(){return"wagtxt"}})(p))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(p))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(p))(d.value)(x1)({wagtxt:L(or(ue)(`run2_
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
          (add <$> (bang 0.0 <|> sl1))`)),txt:L(or(ue)(T1)),ex1:L(rt(Ya()(k)(Nu({reflectSymbol:function(){return"slider"}})()()()(ur({reflectSymbol:function(){return"s0"}})()()(ur({reflectSymbol:function(){return"s1"}})()()(ur({reflectSymbol:function(){return"s2"}})()()(Gn)()()()())()()()())()()()())(Nu({reflectSymbol:function(){return"startStop"}})()()()(ur({reflectSymbol:function(){return"loading"}})()()(ur({reflectSymbol:function(){return"start"}})()()(ur({reflectSymbol:function(){return"stop"}})()()(Gn)()()()())()()()())()()()())(Gn)()()()())()()()())(d.value)(function(n){return function(a){var u=N(I(f))(a.startStop.start)(Y(h(k))(void 0)),i=function(o){return le(Wf(Ct(gt()(J(J(J(J(kt)(Rc)()()()({reflectSymbol:function(){return"playbackRate"}}))(h_)()()()({reflectSymbol:function(){return"loopStart"}}))(S_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Lf)()()()({reflectSymbol:function(){return"buffer"}})))(At()())))({buffer:o,playbackRate:2.6,loopStart:.6,loopEnd:1.1})(jo(I(f))(_t())(function(){return jo(I(f))(_(g)(function(){var m=oa()(Cs),s=Pa(0)(.2)(100)(5);return function(l){return m(s(l))}}())(a.slider.s0))(function(){return jo(I(f))(_(g)(function(){var m=yS(),s=Pa(0)(0)(100)(1.2);return function(l){return m(s(l))}}())(a.slider.s1))(function(){return _(g)(function(){var m=AS(),s=Pa(0)(.05)(100)(1);return function(l){return m(s(l))}}())($n(h(k))(a.slider.s2)(_(g)(We(ga))(N(I(f))(Y(h(k))(0))(a.slider.s1))))})})}))};return Ue(p)(bt(Sn)(_($e)(function(o){return Ue(p)([or(ue)(o.l),Eo(p)(Q_(fu)(S(f))(Y(h(k)))(Va(fu)(Z(Jo)(Co.value)("range"))(function(){return Va(fu)(Z(cf)(Ti.value)("0"))(function(){return Va(fu)(Z(ff)(hi.value)("100"))(function(){return Va(fu)(Z(_f)(Fi.value)("1"))(function(){return Va(X_)(Z(pf)($i.value)("50"))(function(){return Z(lf)(xi.value)(Ye(function(){var m=vr(f)(Ke)(Kf(hn)(zf)(o.f)),s=Qn(Sa)(Yc);return function(l){return m(s(ul(l)))}}()))})})})})})))([])])})([{l:"Playback rate",f:n.slider.s0},{l:"Loop start",f:n.slider.s1},{l:"Loop end",f:n.slider.s2}]))([gn(p)(Q_(fu)(S(f))(_(g)(function(){var o=Z(dr)(mr.value);return function(m){return o(Ye(x(m)))}}()))(Va(fu)(Q(g)(a.startStop.loading)(T(f)(void 0)))(function(){return Va(X_)(Qe(g)(a.startStop.stop)(function(o){return X(nt)(o)(X(nt)(t(T(f)(void 0)))(n.startStop.start(void 0)))}))(function(){return Qe(g)($n(h(k))(N(I(f))(Y(h(k))(T(f)(void 0)))(_(g)(function(o){return o.value0})(r)))(Q(g)(u)(tt(et))))(function(o){return function(){o(),n.startStop.loading(void 0)();var s=ko(q(Lr)(ua(Nr))(function(l){return q(Lr)(Hu(Nr)(l))(function(v){return q(Lr)(Et(l)(F1))(function(D){return br(Nr)(function(){var C=Dt(l)([i(D)])(),ut=X(nt)(X(nt)(C)(v))(Cn(fr)(l));return n.startStop.stop(ut)(),ut})})})}))();return t(function(){return n.startStop.start(void 0)(),Ho(gi(s))()})(),void 0}})})})))([un(ue)(jo(I(f))(_(g)(x("Turn off"))(a.startStop.stop))(function(){return _(g)(x("Turn on"))(u)}))])]))}})))})}}};var M1=`module Main where

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
  )`,w1=iu(function(t){return Wt(function(e){return $t(t)(function(r){return function(){var a=Bu();return e(r(a))()}})})}),P1=function(){return d.value}(),O1=function(t){if(t<.142857)return 261.625565;if(t<.285714)return 293.664768;if(t<.428571)return 349.228231;if(t<.571429)return 391.995436;if(t<.714286)return 440;if(t<.857143)return 523.251131;if(nr)return 587.329536;throw new Error("Failed pattern match at WAGS.Example.Docs.Events.Ex2 (line 225, column 1 - line 225, column 23): "+[t.constructor.name])},R0=function(t){return function(e){return function(r){return pe({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(p)(W()(W()(K(f))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(p))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}})(p))(d.value)(P1)({txt:L(or(ue)(M1)),ex2:L(rt(Ya()(k)(ur({reflectSymbol:function(){return"slider"}})()()(Nu({reflectSymbol:function(){return"startStop"}})()()()(ur({reflectSymbol:function(){return"start"}})()()(ur({reflectSymbol:function(){return"stop"}})()()(Gn)()()()())()()()())(Gn)()()()())()()()())(d.value)(function(n){return function(a){var u=N(I(f))(a.startStop.start)(Y(h(k))(void 0)),i=function(o){return ji(k)(o)(function(m){var s=_(g)(function(){var ut=We(ga)(.01);return function(mt){return ut(rn(mt))}}())(m),l=_(g)(Ba)(m),v=N(I(f))(_t())(_(g)(function(){var ut=go()(Cs);return function(mt){return ut(O1(mt))}}())(l)),D=_(g)(function(ut){return ks(function(mt){return{p:[0,.15,.05,.01,.005,5e-4,0],d:.4,o:mt}}(ut))})(s),c=_(g)(function(ut){return ks(function(mt){return{p:[0,.3,.1,.05,.01,.005,0],d:.4,o:mt}}(ut))})(s),C=_(g)(function(ut){return ks(function(mt){return{p:[0,.6,.2,.1,.5,.03,0],d:.4,o:mt}}(ut))})(s);return[Fa(Os(Es)(0)(v))(function(ut){return function(mt){return Ot(ft)(2)([qr(ft)(0)(_(g)(En()(wn))(C))([kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:1e3,q:20})([ut])]),qr(ft)(0)(_(g)(En()(wn))(c))([kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:2e3,q:20})([ut])]),qr(ft)(0)(_(g)(En()(wn))(D))([Wc(GD(Ct(gt()(J(J(kt)(UC)()()()({reflectSymbol:function(){return"q"}}))(FD)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:4e3,q:20})([ut])])])}})]})};return Ue(p)([Ue(p)([or(ue)("tempo"),Eo(p)(Q_(fu)(S(f))(Y(h(k)))(Va(fu)(Z(Jo)(Co.value)("range"))(function(){return Va(fu)(Z(cf)(Ti.value)("0"))(function(){return Va(fu)(Z(ff)(hi.value)("100"))(function(){return Va(fu)(Z(_f)(Fi.value)("1"))(function(){return Va(X_)(Z(pf)($i.value)("50"))(function(){return Z(lf)(xi.value)(Ye(function(){var o=vr(f)(Ke)(Kf(hn)(zf)(n.slider)),m=Qn(Sa)(Yc);return function(s){return o(m(ul(s)))}}()))})})})})})))([])]),gn(p)(sn(zt)(S(f))(_(g)(function(){var o=Z(dr)(mr.value);return function(m){return o(Ye(x(m)))}}()))([Qe(g)($n(h(k))(N(I(f))(Y(h(k))(T(f)(void 0)))(_(g)(function(o){return o.value0})(r)))(Q(g)(u)(tt(et))))(function(o){return function(){o();var s=ua(fr)(),l=Si(h(k))(at.create)(w1)(XS(s)(.91)(_(g)(Pa(0)(.42)(100)(1.4))(a.slider))),v=jc(s)(i(l))(),D=X(nt)(v)(Cn(fr)(s));return t(X(nt)(D)(n.startStop.start(void 0)))(),n.startStop.stop(X(nt)(D)(Cn(fr)(s)))()}}),Qe(g)(a.startStop.stop)(function(o){return X(nt)(o)(X(nt)(t(T(f)(void 0)))(n.startStop.start(void 0)))})]))([un(ue)(Sr(zt)(S(f))([Q(g)(u)("Turn on"),Q(g)(a.startStop.stop)("Turn off")]))])])}})))})}}};var R1=function(){return d.value}(),N0=function(){return pe({reflectType:function(){return`<section>
  <h2>Three flavors of events.</h2>

  <p>When we're in the browser, events tend to come in three broad categories:</p>

  <ul>
    <li>Things that need to happen <span style="font-weight: 800;">now</span>.</li>
    <li>Things that happen as the result of a user interaction.</li>
    <li>Things that are scheduled to happen in the future, for example with <code>setTimeout</code>.</li>
  </ul>

  <p>The next three examples cover all three cases.</p>

</section>`}})({reflectType:function(){return"@"}})()()(p)(K(f))(d.value)(R1)({})}();var L1=function(){return d.value}(),L0=function(){return pe({reflectType:function(){return`<section>
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
</section>`}})({reflectType:function(){return"@"}})()()(p)(K(f))(d.value)(L1)({})}();var B1=function(){return d.value}(),W0=function(){return pe({reflectType:function(){return`<section>

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
</section>`}})({reflectType:function(){return"@"}})()()(p)(K(f))(d.value)(B1)({})}();var U1=function(){return d.value}(),B0=function(t){return function(e){return function(r){return function(n){var a=function(i){return Ma(I(f))(h(k))(n)(X(nt)(e(i))(ln))},u=wa(t)(r);return pe({reflectType:function(){return`<div>
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
</div>`}})({reflectType:function(){return"@"}})()()(p)(W()(W()(W()(W()(W()(on()(W()(K(f))({reflectType:function(){return"primer"}})({reflectSymbol:function(){return"primer"}})(p))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(p))({reflectType:function(){return"inWags"}})({reflectSymbol:function(){return"inWags"}})(p))({reflectType:function(){return"flavors"}})({reflectSymbol:function(){return"flavors"}})(p))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}})(p))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(p))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}})(p))(d.value)(U1)({next:a(L_.value),primer:L(W0),inWags:L(L0),flavors:L(N0),ex0:L($0(u)(e)(n)),ex1:L(I0(u)(e)(n)),ex2:L(R0(u)(e)(n))})}}}};var H1=function(){return d.value}(),G0=function(t){return function(e){return function(r){return pe({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(p)(W()(K(f))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(p))(d.value)(H1)({ai0:L(rt(ht(r)(t)(function(n){return Ao(On)(Ut(ki)(Ut(ki)(Ut(ki)(_(zc)(function(a){return function(u){return function(i){return function(o){return{tink0:a,tink1:u,tink2:i,tink3:o}}}}})(Pn(On)(Et(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Pn(On)(Et(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Pn(On)(Et(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(Pn(On)(Et(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(a){return Dt(n)([Ot(ft)(1)(function(){var u=function(i){return Y(h(k))(Vf()(Rf)(gs()(We(ga)(i))(g_)))};return[jn(Ha)(a.tink0)(u(.1)),jn(Ha)(a.tink1)(u(.2)),jn(Ha)(a.tink2)(u(.9)),jn(Ha)(a.tink3)(u(1.8))]}())])}})))})}}};var V1=function(){return d.value}(),U0=function(t){return function(e){return function(r){return pe({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(p)(W()(K(f))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(p))(d.value)(V1)({ai0:L(rt(ht(r)(t)(function(n){return Ao(On)(Ut(ki)(Ut(ki)(Ut(ki)(_(zc)(function(a){return function(u){return function(i){return function(o){return{tink0:a,tink1:u,tink2:i,tink3:o}}}}})(Pn(On)(Et(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Pn(On)(Et(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Pn(On)(Et(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(Pn(On)(Et(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(a){return Dt(n)([Ot(ft)(1)(function(){var u=function(o){return Y(h(k))(Vf()(Rf)(gs()(We(ga)(o))(g_)))},i=function(o){var m=su(xo)(o)(4);return m===0?a.tink0:m===1?a.tink1:m===2?a.tink2:a.tink3};return Qe($e)(pn(0)(100))(function(o){var m=ze(o);return jn(Ha)(i(o))(u(.3+.3*(m*Yi(1.005)(m))))})}())])}})))})}}};var j1=function(){return d.value}(),q0=function(t){return function(e){return function(r){return pe({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(p)(W()(K(f))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(p))(d.value)(j1)({ai0:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([Fa(le(Ce)(a)(_t()))(function(u){return function(i){return Ot(ft)(.8)([kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:400,q:1})([u]),kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:880,q:5})([u]),kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:1200,q:10})([u]),kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:2e3,q:20})([u]),kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:3e3,q:30})([u])])}})])}})))})}}};var Q1=function(){return d.value}(),H0=function(t){return function(e){return function(r){return pe({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(p)(W()(K(f))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(p))(d.value)(Q1)({ai0:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([Fa(le(Ce)(a)(_t()))(function(u){return function(i){return Ot(ft)(.8)(Qe($e)(pn(0)(40))(Ka(Bn)(ze)(function(o){return kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:200+o*150,q:30})([u])})))}})])}})))})}}};var Y1=function(){return d.value}(),z0=function(t){return function(e){return function(r){return pe({reflectType:function(){return`<div>
  <pre><code>\\buf -> run2_
  [ fix
      \\b -> gain_ 1.0
        [ playBuf buf bangOn
        , delay_ 0.1 [ gain_ 0.6 [ b ] ]
        ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(p)(W()(K(f))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(p))(d.value)(Y1)({ai0:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(a){return Dt(n)([eu(function(u){return Ot(ft)(1)([jn(Ha)(a)(_t()),yo(tn)(.1)([Ot(ft)(.6)([u])])])})])}})))})}}};var tL=function(){return d.value}(),eL=function(t){return function(e){return Y(t)(En(e)(wn)({p:[1,1,0],o:0,d:10}))}},rL=function(t){return function(e){return Y(t)(En(e)(wn)({p:[1,1,0],o:0,d:8}))}},ol=function(t){return function(e){return function(r){return function(n){return function(a){return function(u){return function(i){return yo(t)(n)([Ot(e)(a)([Wc(r)(u)(i)])])}}}}}}},V0=function(t){return function(e){return function(r){return pe({reflectType:function(){return`<div>
  <pre><code>@txt@</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(p)(W()(W()(K(f))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(p))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(p))(d.value)(tL)({txt:L(or(ue)(`dgh d g h i =
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
  ]`)),ai0:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(a){return Dt(n)([Fa(jn(Ha)(a)(_t()))(function(u){return function(i){return eu(function(o){return Ot(ft)(1)([u,ol(tn)(ft)(tu)(.15)(.7)(1500)([eu(function(m){return qr(ft)(1)(eL(h(k))())([ol(tn)(ft)(tu)(.4)(.5)(2500)([o,m])])})]),ol(tn)(ft)(tu)(.29)(.85)(2e3)([eu(function(m){return Ot(ft)(1)([ol(tn)(ft)(tu)(.6)(.6)(3500)([o,eu(function(s){return qr(ft)(1)(rL(h(k))())([ol(tn)(ft)(tu)(.75)(.6)(4e3)([m,s]),ol(tn)(ft)(tu)(.75)(.55)(3e3)([u])])})])])})])])})}})])}})))})}}};var aL=function(){return d.value}(),J0=function(t){return function(e){return function(r){return function(n){var a=function(u){return Ma(I(f))(h(k))(n)(X(nt)(e(u))(ln))};return pe({reflectType:function(){return`<section>
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
  </section>`}})({reflectType:function(){return"@"}})()()(p)(on()(K(f))({reflectType:function(){return"hwLink"}})({reflectSymbol:function(){return"hwLink"}})(p))(d.value)(aL)({hwLink:a(Uf.value)})}}}};var oL=function(){return d.value}(),j0=function(t){return function(e){return function(r){return function(n){var a=function(i){return Ma(I(f))(h(k))(n)(X(nt)(e(i))(ln))},u=wa(t)(r);return pe({reflectType:function(){return`<div>
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
</div>`}})({reflectType:function(){return"@"}})()()(p)(W()(W()(W()(W()(W()(W()(W()(on()(K(f))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(p))({reflectType:function(){return"intro"}})({reflectSymbol:function(){return"intro"}})(p))({reflectType:function(){return"code5"}})({reflectSymbol:function(){return"code5"}})(p))({reflectType:function(){return"code4"}})({reflectSymbol:function(){return"code4"}})(p))({reflectType:function(){return"code3"}})({reflectSymbol:function(){return"code3"}})(p))({reflectType:function(){return"code2"}})({reflectSymbol:function(){return"code2"}})(p))({reflectType:function(){return"code1"}})({reflectSymbol:function(){return"code1"}})(p))({reflectType:function(){return"code0"}})({reflectSymbol:function(){return"code0"}})(p))(d.value)(oL)({intro:L(J0(t)(e)(r)(n)),next:a(R_.value),code0:L(G0(u)(e)(n)),code1:L(U0(u)(e)(n)),code2:L(q0(u)(e)(n)),code3:L(H0(u)(e)(n)),code4:L(z0(u)(e)(n)),code5:L(V0(u)(e)(n))})}}}};var X0=function(t){return function(e){return function(r){return new O(j(t)("code")(e)(G(r)))}}},Ob=function(t){return X0(t)(w(S(t.MonadST5().Monad0().Applicative0())))};var Q0=function(t){return function(e){return function(r){return new O(j(t)("pre")(e)(G(r)))}}},Ib=function(t){return Q0(t)(w(S(t.MonadST5().Monad0().Applicative0())))};var lL=function(){return d.value}(),K0=function(t){return function(e){return function(r){return function(n){var a=X(nt)(e(N_.value))(ln),u=wa(t)(r);return pe({reflectType:function(){return`<div>
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
</div>`}})({reflectType:function(){return"@"}})()()(p)(W()(on()(W()(K(f))({reflectType:function(){return"result"}})({reflectSymbol:function(){return"result"}})(p))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(p))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}})(p))(d.value)(lL)({code:L(Ib(p)([Ob(p)([or(ue)(`case e of
  Just x -> x *> push Nothing
  _ -> (run2_ [ gain_ 0.15 [ sinOsc 440.0 bangOn ] ]
         >>= Just >>> push`)])])),result:L(rt(ht(n)(u)(function(i){return T(ya)(void 0)})(function(i){return function(o){return Dt(i)([Ot(ft)(.15)([Gf(rf)(440)(_t())])])}}))),next:Ma(I(f))(h(k))(n)(a)})}}}};var Y0=vc;var Z0=function(){return function(t){return t}};var th=function(){return function(t){return t}};var Rb=function(){function t(){}return t.value=new t,t}();var eh={attr:function(t){return function(e){return b({key:"height",value:U(e)})}}};var Nb=function(){function t(){}return t.value=new t,t}();var rh={attr:function(t){return function(e){return b({key:"width",value:U(e)})}}};var Lb=function(t){return function(e){return function(r){return new O(j(t)("canvas")(e)(G(r)))}}};var Wb=function(){function t(){}return t.value=new t,t}(),Bb={attr:function(t){return function(e){return b({key:"@self@",value:ct(e)})}}};function om(t){return function(){return t.getContext("2d")}}function K_(t){return function(e){return function(){t.fillStyle=e}}}function im(t){return function(){t.beginPath()}}function fm(t){return function(){t.fill()}}function Gb(t){return function(e){return function(){t.arc(e.x,e.y,e.radius,e.start,e.end,e.useCounterClockwise)}}}function cm(t){return function(e){return function(){t.fillRect(e.x,e.y,e.width,e.height)}}}var xL=function(){return 2*Zi}(),il=function(t){return{o:t.value0+.04,n:t.value1,t:Go}};var FL=function(){return d.value}(),fl=function(t){return function(e){return function(r){return function(n){return Y(t)(go(e)(wn)({p:[r,n],o:0,d:16}))}}}},$L=function(t){return function(e){return Y(t)(En(e)(wn)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:18}))}},ML=function(t){return function(e){return Y(t)(En(e)(wn)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:24}))}};var lm=function(t){return function(e){return function(r){return function(n){return function(a){return function(u){return function(i){return function(o){return function(m){return function(s){return w_(t)(n)(a)([qr(e)(u)(i)([jD(r)(o)(m)(s)])])}}}}}}}}}},nh=function(t){return function(e){return function(r){return function(n){return function(a){return function(u){return function(i){return function(o){return function(m){return function(s){return w_(t)(n)(a)([qr(e)(u)(i)([JD(r)(o)(m)(s)])])}}}}}}}}}},wL=function(t){return function(e){return function(r){return function(n){return Y(t)(Zc(e)(wn)({p:[r,n],o:0,d:16}))}}}},ah=400,Ub=ze(ah),PL=function(){return Ht(Xa)(ah)+"px"}(),uh=600,qb=ze(uh),OL=function(){return Ht(Xa)(uh)+"px"}(),IL={pluck0:"https://freesound.org/data/previews/493/493016_10350281-lq.mp3",pluck1:"https://freesound.org/data/previews/141/141524_2558140-lq.mp3",strum0:"https://freesound.org/data/previews/234/234738_3635427-lq.mp3"},oh=function(t){return function(e){return function(r){return pe({reflectType:function(){return"<section>@ex1@</section>"}})({reflectType:function(){return"@"}})()()(p)(W()(K(f))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(p))(d.value)(FL)({ex1:L(rt(Ya()(k)(ur({reflectSymbol:function(){return"canvas"}})()()(ur({reflectSymbol:function(){return"slider"}})()()(Nu({reflectSymbol:function(){return"startStop"}})()()()(ur({reflectSymbol:function(){return"loading"}})()()(ur({reflectSymbol:function(){return"start"}})()()(ur({reflectSymbol:function(){return"stop"}})()()(Gn)()()()())()()()())()()()())(Gn)()()()())()()()())()()()())(d.value)(function(n){return function(a){var u=N(I(f))(Y(h(k))(void 0))(a.startStop.start),i=function(o){return function(m){return function(s){var l=_(g)(function(v){return new at(v.acTime,v.value)})(el(o)(a.slider));return[Ps(ws(Ct(gt()(J(J(kt)(Ms)()()()({reflectSymbol:function(){return"fftSize"}}))($s)()()()({reflectSymbol:function(){return"cb"}})))(At()())))({cb:function(v){return function(){return Tn(new B(v))(s)(),Tn(V.value)(s)}},fftSize:ys.value})(T(pr)(Fa(jn(Ha)(m)(N(I(f))(_t())(_(g)(function(){var v=oa()(Wr),D=Vn(bn)(Pa(0)(.96)(100)(1.04));return function(c){return v(il(D(c)))}}())(l))))(function(v){return function(D){return eu(function(c){return Ot(ft)(1)([v,w_(UD(Ct(gt()(J(J(kt)(qC)()()()({reflectSymbol:function(){return"maxDelayTime"}}))($D)()()()({reflectSymbol:function(){return"delayTime"}})))(At()())))({maxDelayTime:2.5,delayTime:1})(_(g)(function(){var C=Zc()(Wr),ut=Vn(bn)(Pa(0)(.5)(100)(2.45));return function(mt){return C(il(ut(mt)))}}())(l))([qr(ft)(.4)(_(g)(function(){var C=En()(Wr),ut=Vn(bn)(Pa(0)(.6)(100)(.9));return function(mt){return C(il(ut(mt)))}}())(l))([v])]),lm(tn)(ft)(tu)(.15)(w(S(f)))(.7)(w(S(f)))(1500)(fl(h(k))()(1500)(3e3))([eu(function(C){return qr(ft)(1)($L(h(k))())([lm(tn)(ft)(tu)(.4)(w(S(f)))(.5)(w(S(f)))(3e3)(fl(h(k))()(3e3)(100))([c,C])])})]),lm(tn)(ft)(tu)(.29)(_(g)(function(){var C=Zc()(Wr),ut=Vn(bn)(Pa(0)(.1)(100)(.4));return function(mt){return C(il(ut(mt)))}}())(l))(.85)(w(S(f)))(2e3)(fl(h(k))()(2e3)(5e3))([eu(function(C){return Ot(ft)(1)([lm(tn)(ft)(tu)(.6)(_(g)(function(){var ut=Zc()(Wr),mt=Vn(bn)(Pa(0)(.8)(100)(.3));return function(ve){return ut(il(mt(ve)))}}())(l))(.6)(w(S(f)))(3500)(fl(h(k))()(3500)(100))([c,eu(function(ut){return qr(ft)(1)(ML(h(k))())([nh(tn)(ft)(qD)(.75)(_(g)(function(){var mt=Zc()(Wr),ve=Vn(bn)(Pa(0)(.9)(100)(.1));return function(ne){return mt(il(ve(ne)))}}())(l))(.6)(w(S(f)))(4e3)(fl(h(k))()(4e3)(200))([C,ut]),nh(tn)(ft)(qD)(.75)(wL(h(k))()(.75)(.2))(.55)(w(S(f)))(200)(fl(h(k))()(200)(4e3))([v])])})])])})])])})}})))]}}};return Ue(p)([Lb(p)(N(I(f))(sn(zt)(S(f))(Y(h(k)))([Z(rh)(Nb.value)(OL),Z(eh)(Rb.value)(PL),Z(hk)(Vt.value)("width: 100%;"),Z(Bb)(Wb.value)(function(){var o=vr(f)(Ke)(function(m){return function(){var l=om(m)();return K_(l)("black")(),cm(l)({width:qb,height:Ub,x:0,y:0})(),void 0}});return function(m){return o(pb(m))}}())]))(_(g)(function(o){return Z(Bb)(Wb.value)(function(){var m=vr(f)(Ke)(function(s){return function(){var v=om(s)();return K_(v)("black")(),cm(v)({width:qb,height:Ub,x:0,y:0})(),K_(v)("rgba(255,255,255,0.2)")(),Al(o)(function(D){return function(){return im(v)(),Gb(v)({end:xL,radius:D.value1*40,start:0,x:D.value0.x*qb,y:D.value0.y*Ub,useCounterClockwise:!1})(),fm(v)()}})()}});return function(s){return m(pb(s))}}())})(a.canvas)))([]),Eo(p)(sn(zt)(S(f))(Y(h(k)))([Z(Jo)(Co.value)("range"),Z(cf)(Ti.value)("0"),Z(ff)(hi.value)("100"),Z(_f)(Fi.value)("1"),Z(pf)($i.value)("50"),Z(Sk)(Vt.value)("width: 100%;"),Z(lf)(xi.value)(Ye(function(){var o=vr(f)(Ke)(Kf(hn)(zf)(n.slider)),m=Qn(Sa)(Yc);return function(s){return o(m(ul(s)))}}()))]))([]),gn(p)(Sr(zt)(S(f))([Y(h(k))(Z(Dc)(Vt.value)("width:100%; padding:1.0rem;")),sn(zt)(S(f))(_(g)(function(){var o=Z(dr)(mr.value);return function(m){return o(Ye(x(m)))}}()))([Q(g)(a.startStop.loading)(T(f)(void 0)),Qe(g)(a.startStop.stop)(function(o){return X(nt)(o)(X(nt)(t(T(f)(void 0)))(n.startStop.start(void 0)))}),Qe(g)($n(h(k))(N(I(f))(Y(h(k))(T(f)(void 0)))(_(g)(function(o){return o.value0})(r)))(Q(g)(u)(tt(et))))(function(o){return function(){o(),n.startStop.loading(void 0)();var s=sr(V.value)(),l=ko(q(Lr)(ua(Nr))(function(v){return q(Lr)(Hu(Nr)(v))(function(D){return q(Lr)(_(Ai)(th())(LE(On)(Y0)(Et(v))(Z0()(IL))))(function(c){return q(Lr)(br(Nr)(r_(0)(5e4)))(function(C){var ut=Sc(Zv(Ea(u_(c.pluck0))(gc(Av(kv()(c))))))({newSeed:kc(C),size:4});return br(Nr)(function(){var ve=Wn(ei)(f)(function(pt){return function(){var he=Bu(),ae=Bu();return{x:he,y:ae}}})(pn(0)(127))(),ne=Dt(v)(i(v)(ut)(s))(),Ze=$t(af)(function(pt){return function(){var he=Rr(s)();return _a(f)(Ke)(he)(function(ae){return function(){var So=G_(ae)(),zu=_(F)(function(){var Mi=wl(ve),cu=_($e)(function(_n){return function(Xo){return Xo/255}(_n)});return function(_n){return Mi(cu(_n))}}())(zs(Hs)(So))();return n.canvas(zu)(),void 0}})()}})(),lr=X(nt)(X(nt)(X(nt)(ne)(D))(Cn(fr)(v)))(Ze);return n.startStop.stop(lr)(),lr})})})})}))();return t(function(){return n.startStop.start(void 0)(),Ho(gi(l))()})(),void 0}})])]))([un(ue)(Sr(zt)(S(f))([_(g)(x("Turn off"))(a.startStop.stop),_(g)(x("Turn on"))(u),_(g)(x("Loading..."))(a.startStop.loading)]))])])}})))})}}};var NL=function(){return d.value}(),ih=function(t){return function(e){return function(r){return function(n){var a=wa(t)(r);return wt({reflectType:function(){return`<div>
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
</div>`}})()()(W()(on()(K(f))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(p))({reflectType:function(){return"ex"}})({reflectSymbol:function(){return"ex"}})(p))(p)(NL)({next:Ma(I(f))(h(k))(n)(X(nt)(e(Uf.value))(ln)),ex:L(oh(a)(e)(n))})}}}};var WL=function(){return d.value}(),fh=function(t){return function(e){return function(r){return function(n){return wt({reflectType:function(){return`<div>
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
</div>`}})()()(on()(K(f))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(p))(p)(WL)({next:Y(h(k))(Z(dr)(mr.value)(Ye(x(X(nt)(e(Rs.value))(ln)))))})}}}};var GL=function(){return d.value}(),ch=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
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
`}})()()(W()(W()(K(f))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(p))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}})(p))(p)(GL)({txt:L(or(ue)(`\\ctx buf -> run2 ctx
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
  ]`)),cancel:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([Ot(ft)(1)([le(Ce)(a)(Sr(zt)(S(f))([_t(),Ru(1e3)(Mt(f)(oa()(wn)({p:Ja(Zo)(Q($e)(pn(0)(60))([1,1.2,1,.8])),o:1.5,d:30}))),Ru(3e3)(Mt(f)(oa()(FC)({o:3.5})))]))])])}})))})}}};var qL=function(){return d.value}(),lh=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
  <h2>Envelope</h2>
  <p>The <code>AudioEnvelope</code> parameter corresponds to the Web Audio API's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setValueCurveAtTime"><code>setValueCurveAtTime</code></a> function and sets an envelope <code>p</code> over the duration <code>d</code> starting at time <code>o</code>.</p>
  <pre><code>~txt~</code></pre>
  ~envelope~
  </section>
`}})()()(W()(W()(K(f))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(p))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}})(p))(p)(qL)({txt:L(or(ue)(`\\ctx buf -> run2 ctx
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
  ]`)),envelope:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([Ot(ft)(1)([le(Ce)(a)(Sr(zt)(S(f))([_t(),Ru(1e3)(Mt(f)(oa()(wn)({p:Ja(Zo)(Q($e)(pn(0)(60))([1,1.2,1,.8])),o:1.5,d:30})))]))])])}})))})}}};var zL=function(){return d.value}(),_h=function(t){return function(e){return function(r){return pe({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(p)(W()(K(f))({reflectType:function(){return"numericEx"}})({reflectSymbol:function(){return"numericEx"}})(p))(d.value)(zL)({numericEx:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([Ot(ft)(1)([le(Ce)(a)(jo(I(f))(_t())(function(){return jo(I(f))(Ru(1e3)(jo(I(f))(Mt(f)(oa()(Wr)({n:1,o:1,t:SD})))(function(){return Mt(f)(oa()(Wr)({n:1.3,o:2,t:Go}))})))(function(){return Ru(2500)(jo(I(f))(Mt(f)(oa()(Wr)({n:1,o:2.5,t:SD})))(function(){return Mt(f)(oa()(Wr)({n:.7,o:3.5,t:xC}))}))})}))])])}})))})}}};var JL=function(){return d.value}(),ph=function(t){return function(e){return function(r){return pe({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(p)(W()(K(f))({reflectType:function(){return"suddenEx"}})({reflectSymbol:function(){return"suddenEx"}})(p))(d.value)(JL)({suddenEx:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([Ot(ft)(1)([le(Ce)(a)(Sr(zt)(S(f))([_t(),Ru(1500)(Mt(f)(oa()(hC)({n:1.4})))]))])])}})))})}}};var XL=function(){return d.value}(),sh=function(t){return function(e){return function(r){return wt({reflectType:function(){return`<section>
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
`}})()()(W()(K(f))({reflectType:function(){return"unitEx"}})({reflectSymbol:function(){return"unitEx"}})(p))(p)(XL)({unitEx:L(rt(ht(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([le(Ce)(a)(Sr(zt)(S(f))([_t(),Mt(f)(oa()(CC(mi)(mi))(gC(Ot(ft)(1)([Is(Ss)(1)(_t()),Ot(ft)(.2)([Bc(Ts)(100)([$_($c)(50)(_t())])])]))))]))])}})))})}}};var KL=function(){return d.value}(),mh=function(t){return function(e){return function(r){return function(n){var a=X(nt)(e(W_.value))(ln),u=wa(t)(r);return wt({reflectType:function(){return`<div>
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
</div>`}})()()(W()(W()(on()(W()(W()(W()(K(f))({reflectType:function(){return"unit"}})({reflectSymbol:function(){return"unit"}})(p))({reflectType:function(){return"sudden"}})({reflectSymbol:function(){return"sudden"}})(p))({reflectType:function(){return"numeric"}})({reflectSymbol:function(){return"numeric"}})(p))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(p))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}})(p))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}})(p))(p)(KL)({sudden:L(ph(u)(e)(n)),numeric:L(_h(u)(e)(n)),envelope:L(lh(u)(e)(n)),cancel:L(ch(u)(e)(n)),unit:L(sh(u)(e)(n)),next:Ma(I(f))(h(k))(n)(a)})}}}};var ZL=function(){return d.value}(),vh=function(t){return function(e){return function(r){return function(n){return wt({reflectType:function(){return`<div>
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
</div>`}})()()(on()(K(f))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(p))(p)(ZL)({next:Y(h(k))(Z(dr)(mr.value)(Ye(x(X(nt)(e(Ns.value))(ln)))))})}}}};var eW=function(){return d.value}(),Dh=function(t){return function(e){return function(r){return function(n){return wt({reflectType:function(){return`<div>
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
</div>`}})()()(on()(K(f))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(p))(p)(eW)({next:Y(h(k))(Z(dr)(mr.value)(Ye(x(X(nt)(e(qf.value))(ln)))))})}}}};var nW=function(){return d.value}(),dh=function(t){return function(e){return function(r){return function(n){return pe({reflectType:function(){return`<div>
  <h1>Imperative API</h1>

  <h2>Like JavaScript, but PureScript</h2>
  <p>
    If you're coming from the JavaScript or TypeScript world, or if you're a fan of monadic <code>do</code> notation, you may enjoy building things step-by-step rather than constructing large declarative structures. If you're that sort of person, this section is for you!
  </p>

  <h2>Parting shot</h2>
  <p>Thanks for checking out wags! We want it to be the most ergonomimc, expressive, and performant Web Audio API on your side of the Mississippi. It certainly is for me, and as I'm in Finland, I'm on <i>both sides</i> of the Mississippi, so you can't beat that! If you have any questions, comments, concerns or would just like to say "hi!", please check out the <a href="https://github.com/mikesol/purescript-wags">Wags GitHub Repo</a> or the <a href="https://purescript.org/chat">PureScript Discord's music channel</a>. Happy wagging!</p>
</div>`}})({reflectType:function(){return"~"}})()()(p)(K(f))(d.value)(nW)({})}}}};var uW=`module Main where

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
`,oW=iu(function(t){return Wt(function(e){return $t(t)(function(r){return function(){var a=Bu();return e(r(a))()}})})}),iW=function(){return d.value}(),fW="https://freesound.org/data/previews/339/339810_5121236-lq.mp3",bh=function(t){return function(e){return function(r){return pe({reflectType:function(){return`<section>
  <h2>Hello subgraph</h2>

  <p>Subgraphs have the type <code>Event (Event (Channel outputChannels lock payload))</code>. Streaming audio is a data type with two constructors: <code>sound</code> to create a subgraph and <code>silence</code> to turn it off. The inner event listens for sound/silence, and the outer event adds subgraphs to the scene. You can create as many subgraphs as you like: wags automatically frees up resources when you send the <code>silence</code> event. Note that, once you turn a subraph off with <code>silence</code>, you can't turn it back on again. In this case, just create a new subgraph.</p>

  <p>Here's a simple subgraph that is connected to a slider. As you slide the slider, new nodes are provisioned. Each one has a pseudo-random pitch.</p>

  <pre><code>@txt@</code></pre>
  @ex1@

</section>
`}})({reflectType:function(){return"@"}})()()(p)(W()(W()(K(f))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(p))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(p))(d.value)(iW)({txt:L(or(ue)(uW)),ex1:L(rt(Ya()(k)(ur({reflectSymbol:function(){return"slider"}})()()(Nu({reflectSymbol:function(){return"startStop"}})()()()(ur({reflectSymbol:function(){return"loading"}})()()(ur({reflectSymbol:function(){return"start"}})()()(ur({reflectSymbol:function(){return"stop"}})()()(Gn)()()()())()()()())()()()())(Gn)()()()())()()()())(d.value)(function(n){return function(a){var u=N(I(f))(Y(h(k))(void 0))(a.startStop.start),i=Si(h(k))(at.create)(oW)(Iu(h(k))(function(m){return function(s){return s+1|0}})(a.slider)(0)),o=function(m){return[Ot(ft)(1)([Ip(_(g)(function(s){return Sr(zt)(S(f))([Y(h(k))(AC(jn(T_(Ct(gt()(J(J(kt)(RC)()()()({reflectSymbol:function(){return"playbackRate"}}))(E_)()()()({reflectSymbol:function(){return"buffer"}})))(At()())))({buffer:m,playbackRate:.7+Ba(s)*2})(_t()))),Ru(5e3)(Y(h(k))(kC))])})(i))])]};return Ue(p)([Ue(p)([or(ue)("Slide me!"),Eo(p)(sn(zt)(S(f))(Y(h(k)))([Z(Jo)(Co.value)("range"),Z(cf)(Ti.value)("0"),Z(ff)(hi.value)("100"),Z(_f)(Fi.value)("1"),Z(pf)($i.value)("50"),Z(lf)(xi.value)(Ye(x(n.slider(void 0))))]))([])]),gn(p)(sn(zt)(S(f))(_(g)(function(){var m=Z(dr)(mr.value);return function(s){return m(Ye(x(s)))}}()))([Q(g)(a.startStop.loading)(T(f)(void 0)),Qe(g)(a.startStop.stop)(function(m){return X(nt)(m)(X(nt)(t(T(f)(void 0)))(n.startStop.start(void 0)))}),Qe(g)($n(h(k))(N(I(f))(Y(h(k))(T(f)(void 0)))(_(g)(function(m){return m.value0})(r)))(Q(g)(u)(tt(et))))(function(m){return function(){m(),n.startStop.loading(void 0)();var l=ko(q(Lr)(ua(Nr))(function(v){return q(Lr)(Hu(Nr)(v))(function(D){return q(Lr)(Et(v)(fW))(function(c){return br(Nr)(function(){var ut=qs(o(c))(),mt=X(nt)(X(nt)(ut)(D))(Cn(fr)(v));return n.startStop.stop(mt)(),mt})})})}))();return t(function(){return n.startStop.start(void 0)(),Ho(gi(l))()})(),void 0}})]))([un(ue)(Sr(zt)(S(f))([_(g)(x("Turn off"))(a.startStop.stop),_(g)(x("Turn on"))(u)]))])])}})))})}}};var lW=function(){return d.value}(),yh=function(t){return function(e){return function(r){return function(n){var a=wa(t)(r);return wt({reflectType:function(){return`<div>
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
</div>`}})()()(W()(W()(K(f))({reflectType:function(){return"suby"}})({reflectSymbol:function(){return"suby"}})(p))({reflectType:function(){return"appl"}})({reflectSymbol:function(){return"appl"}})(p))(p)(lW)({appl:L(rt(Us("\u{1F44F}")(n)(a)(function(u){return Et(u)("https://freesound.org/data/previews/277/277021_1402315-lq.mp3")})(function(u){return function(i){return Dt(u)([Ot(ft)(1)([le(Ce)(i)(_t())])])}}))),suby:L(bh(a)(e)(n))})}}}};var $Lt=function(t){return t},MLt={Coercible0:function(){}},pW=function(t){return function(e){var r=function(a){var u=function(i){if(i instanceof I_)return Ue(p)(T(pr)(rt(Gr(k)(ih(a.setCancellation)(a.setPage)))));if(i instanceof Uf)return Ue(p)(T(pr)(rt(Gr(k)(K0(a.setCancellation)(a.setPage)))));if(i instanceof N_)return Ue(p)(T(pr)(rt(Gr(k)(j0(a.setCancellation)(a.setPage)))));if(i instanceof R_)return Ue(p)(T(pr)(rt(Gr(k)(VS(a.setCancellation)(a.setPage)))));if(i instanceof Rs)return Ue(p)(T(pr)(rt(Gr(k)(Dh(a.setCancellation)(a.setPage)))));if(i instanceof qf)return Ue(p)(T(pr)(rt(Gr(k)(B0(a.setCancellation)(a.setPage)))));if(i instanceof L_)return Ue(p)(T(pr)(rt(Gr(k)(mh(a.setCancellation)(a.setPage)))));if(i instanceof W_)return Ue(p)(T(pr)(rt(Gr(k)(x0(a.setCancellation)(a.setPage)))));if(i instanceof Ns)return Ue(p)(T(pr)(rt(Gr(k)(dh(a.setCancellation)(a.setPage)))));if(i instanceof KE)return Ue(p)(T(pr)(rt(Gr(k)(fh(a.setCancellation)(a.setPage)))));if(i instanceof B_)return Ue(p)(T(pr)(rt(Gr(k)(yh(a.setCancellation)(a.setPage)))));if(i instanceof YE)return Ue(p)(T(pr)(rt(Gr(k)(vh(a.setCancellation)(a.setPage)))));throw new Error("Failed pattern match at WAGS.Example.Docs (line 144, column 5 - line 144, column 80): "+[i.constructor.name])};return u(a.page)},n=Iu(h(k))(function(a){if(a instanceof Vc)return function(u){return{prevPage:new B(u.curPage),curPage:a.value0,cancel:u.cancel,pageChange:!0}};if(a instanceof YD)return function(u){return{cancel:a.value0,pageChange:!1,curPage:u.curPage,prevPage:u.prevPage}};throw new Error("Failed pattern match at WAGS.Example.Docs (line 134, column 7 - line 136, column 75): "+[a.constructor.name])})(e)({prevPage:V.value,curPage:I_.value,cancel:T(f)(void 0),pageChange:!0});return[Ue(p)(_($e)(function(a){return wv(p)([Mv(p)(N(I(f))(sn(zt)(S(f))(Y(h(k)))([Z(dr)(mr.value)(Ye(x(t(new Vc(a.value0))))),Z(Tk)(Vt.value)("cursor:pointer;")]))(_(g)(function(u){return Z(dr)(mr.value)(Ye(x(function(){return u.cancel(),t(new Vc(a.value0))()})))})(Wl(bu(f))(function(){var u=Du(La);return function(i){return u(function(o){return o.pageChange}(i))}}())(n))))([or(ue)(a.value1.value0)]),bc(p)(Y(h(k))(Z(qp)(Vt.value)(function(){return a.value1.value1?"":"display:none;"}())))([or(ue)(" | ")])])})([new at(I_.value,new at("Home",!0)),new at(Uf.value,new at("Hello world",!0)),new at(N_.value,new at("Array, fan, and fix",!0)),new at(R_.value,new at("Audio units",!0)),new at(qf.value,new at("Events",!0)),new at(L_.value,new at("Parameters",!0)),new at(W_.value,new at("State",!0)),new at(B_.value,new at("Subgraphs",!1))])),Ue(p)(T(pr)(Ev(k)(function(a){return r({page:a.curPage,setPage:function(u){return t(Vc.create(u))},setCancellation:function(u){return t(YD.create(u))}})})(Wl(bu(f))(function(a){return a.pageChange})(n))))]}},wLt=function(t){return{page:t,setPage:Pt(Qr(gr(Ir))),setCancellation:Pt(Qr(gr(Ir)))}},PLt=function(){var e=q(hn)(q(hn)(Ei)(mb))(pS)();return _a(f)(Ke)(_(Or)(sS)(e))(function(r){return function(){var a=Pv(),u=sr(0)(),i=zl(k)(k)(),o=Dk(p)(r)(pW(i.push)(i.event))(gg(u));return Ae(F)($t(o)(function(m){return m(a)}))(),i.push(new Vc(I_.value))()}})()};export{$Lt as TopLevelSg,PLt as main,MLt as newtypeTopLevelSg_,wLt as p2tl,pW as scene};
