var by=function(t){return function(r){for(var e=r.length,n=new Array(e),a=0;a<e;a++)n[a]=t(r[a]);return n}};var bi={compose:function(t){return function(r){return function(e){return t(r(e))}}}},Wu=function(t){return t.compose};var rt=function(t){return t.identity},nt={identity:function(t){return t},Semigroupoid0:function(){return bi}};var oe=!0;var Mt=function(t){return function(r){return function(e){return t(e)(r)}}},T=function(t){return function(r){return t}};var Yi=function(t){return function(r){return r(t)}},bf=function(t){return function(r){return t(r)}};var d=function(){function t(){}return t.value=new t,t}();var _=function(t){return t.map},Gr=function(t){return function(r){return function(e){return _(t)(e)(r)}}},cr=function(t){return _(t)(T(void 0))},K=function(t){return function(r){return function(e){return _(t)(T(e))(r)}}},yf=function(t){return function(r){return _(t)(T(r))}};var qn={map:Wu(bi)},$r={map:by},zm=function(t){return function(r){return function(e){return _(t)(function(n){return n(e)})(r)}}};var yy=function(t){return function(r){return t+r}},Ay=function(t){return function(r){return t.length===0?r:r.length===0?t:t.concat(r)}};var ge=function(t){return t.reflectSymbol};var wc=function(t){var r=function(e){var n;function a(u){e=u}for(;;)n=a(e);return n};return r(t)};var Nl=function(t){return function(r){return{}.hasOwnProperty.call(r,t)}},Ga=function(t){return function(r){return r[t]}},lo=function(t){return function(r){return function(e){var n={};for(var a in e)({}).hasOwnProperty.call(e,a)&&(n[a]=e[a]);return n[t]=r,n}}};var ky={append:function(t){return function(r){return void 0}}},Vm={append:yy};var cn={append:Ay};var bt=function(t){return t.append},Gm=function(t){return{append:function(r){return function(e){return function(n){return bt(t)(r(n))(e(n))}}}}};var I=function(t){return t.alt};var gy=function(t){return function(r){for(var e=t.length,n=r.length,a=new Array(e*n),u=0,i=0;i<e;i++)for(var o=t[i],p=0;p<n;p++)a[u++]=o(r[p]);return a}};var Ll={apply:gy,Functor0:function(){return $r}},Wt=function(t){return t.apply};var Q=function(t){return function(r){return function(e){return Wt(t)(_(t.Functor0())(T(rt(nt)))(r))(e)}}},Zn=function(t){return function(r){return function(e){return function(n){return Wt(t)(_(t.Functor0())(r)(e))(n)}}}};var l=function(t){return t.pure};var zn=function(t){return function(r){return function(e){if(r)return e;if(!r)return l(t)(void 0);throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): "+[r.constructor.name,e.constructor.name])}}},Bl=function(t){return function(r){return function(e){return Wt(t.Apply0())(l(t)(r))(e)}}};var de={pure:function(t){return[t]},Apply0:function(){return Ll}};var Cy=function(t){return function(r){for(var e=[],n=0,a=t.length;n<a;n++)Array.prototype.push.apply(e,r(t[n]));return e}};var Xr=function(t){return t.discard};var yi={bind:Cy,Apply0:function(){return Ll}},P=function(t){return t.bind},Vn=function(t){return Mt(P(t))};var Af=function(t){return function(r){return function(e){return function(n){return P(t)(r(n))(e)}}}};var Qr={discard:function(t){return P(t)}};var nu=function(t){return function(r){return P(t)(r)(rt(nt))}};var An=function(t){return function(r){for(var e=t>r?-1:1,n=new Array(e*(r-t)+1),a=t,u=0;a!==r;)n[u++]=a,a+=e;return n[u]=a,n}},bS=function(t){return function(r){if(t<1)return[];var e=new Array(t);return e.fill(r)}},yS=function(t){return function(r){for(var e=[],n=0,a=0;a<t;a++)e[n++]=r;return e}},Tp=typeof Array.prototype.fill=="function"?bS:yS,AS=function(){function t(a,u){this.head=a,this.tail=u}var r={};function e(a){return function(u){return new t(a,u)}}function n(a){for(var u=[],i=0,o=a;o!==r;)u[i++]=o.head,o=o.tail;return u}return function(a){return function(u){return n(a(e)(r)(u))}}}(),Oa=function(t){return t.length};var hy=function(t){return function(r){return function(e){return function(n){return n<0||n>=e.length?r:t(e[n])}}}};var Ey=function(t){return function(r){return function(e){return function(n){for(var a=0,u=n.length;a<u;a++)if(e(n[a]))return t(a);return r}}}};var Ty=function(t){return function(r){return function(e){return function(n){if(e<0||e>=n.length)return r;var a=n.slice();return a.splice(e,1),t(a)}}}};var kS=function(){function t(r,e,n,a,u,i){var o,p,s,f,m,v,c;for(o=u+(i-u>>1),o-u>1&&t(r,e,a,n,u,o),i-o>1&&t(r,e,a,n,o,i),p=u,s=o,f=u;p<o&&s<i;)m=a[p],v=a[s],c=e(r(m)(v)),c>0?(n[f++]=v,++s):(n[f++]=m,++p);for(;p<o;)n[f++]=a[p++];for(;s<i;)n[f++]=a[s++]}return function(r){return function(e){return function(n){var a;return n.length<2?n:(a=n.slice(0),t(r,e,a,n.slice(0),0,n.length),a)}}}}();var kf=function(t){return function(r){return function(e){for(var n=r.length<e.length?r.length:e.length,a=new Array(n),u=0;u<n;u++)a[u]=t(r[u])(e[u]);return a}}};var Sy=function(t){return function(r){return t[r]}};var CS=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}};var xy={defer:function(t){return function(r){return t(void 0)(r)}}},gf=function(t){return t.defer},Xm=function(t){return function(r){var e=CS("go","Control.Lazy",function(){return gf(t)(function(a){return r(e(25))})}),n=e(25);return n}};var _o=function(t){return function(r){return function(e){return P(t.Bind1())(r)(function(n){return P(t.Bind1())(e)(function(a){return l(t.Applicative0())(n(a))})})}}};var hS=String.fromCharCode(65535),ES=String.fromCharCode(0),TS=Number.POSITIVE_INFINITY,SS=Number.NEGATIVE_INFINITY;var Fy=function(t){return function(r){return function(e){return function(n){return function(a){return n<a?t:n===a?r:e}}}}};var Oy=Fy,$y=Fy;var wy=function(t){return function(r){return t===r}};var My=wy,Py=wy;var Hl={eq:Py},tc={eq:My};var nr=function(t){return t.eq};var ar=function(){function t(){}return t.value=new t,t}(),Ar=function(){function t(){}return t.value=new t,t}(),kr=function(){function t(){}return t.value=new t,t}();var Iy=function(t){return function(r){return t-r|0}},Ry=function(t){return function(r){return t-r}};var Ny=function(t){return function(r){return t+r|0}},Ly=function(t){return function(r){return t*r|0}},By=function(t){return function(r){return t+r}},Hy=function(t){return function(r){return t*r}};var ba=function(t){return t.zero};var $a={add:By,zero:0,mul:Hy,one:1},qu={add:Ny,zero:0,mul:Ly,one:1};var ya=function(t){return t.one};var Rn=function(t){return t.mul};var Br=function(t){return t.add};var Tu=function(t){return t.sub};var Mc={sub:Ry,Semiring0:function(){return $a}},Qm={sub:Iy,Semiring0:function(){return qu}};var Ul=function(t){return function(r){return Tu(t)(ba(t.Semiring0()))(r)}};var Ja=function(){return{compare:$y(ar.value)(kr.value)(Ar.value),Eq0:function(){return Hl}}}(),tn=function(){return{compare:Oy(ar.value)(kr.value)(Ar.value),Eq0:function(){return tc}}}();var ur=function(t){return t.compare};var Wy=function(t){return function(r){return function(e){var n=ur(t)(r)(e);return!(n instanceof ar)}}};var Zu=function(t){return function(r){return function(e){var n=ur(t)(r)(e);if(n instanceof ar)return e;if(n instanceof kr||n instanceof Ar)return r;throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): "+[n.constructor.name])}}};var Ym=function(t){return function(r){return function(e){var n=Wy(t)(e)(ba(r.Semiring0()));return n?e:Ul(r)(e)}}};var oa=function(t){return t.top};var Ic={top:2147483647,bottom:-2147483648,Ord0:function(){return tn}};var ia=function(t){return t.bottom};var Zy=function(t){return t.toString()},zy=function(t){var r=t.toString();return isNaN(r+".0")?r:r+".0"};var xp={show:zy},uu={show:Zy};var jt=function(t){return t.show};var W=function(){function t(){}return t.value=new t,t}(),F=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var Jt=function(t){return function(r){return function(e){if(e instanceof W)return t;if(e instanceof F)return r(e.value0);throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}};var Re={map:function(t){return function(r){return r instanceof F?new F(t(r.value0)):W.value}}};var Aa=function(t){return Jt(t)(rt(nt))},fa=function(){return function(t){if(t instanceof F)return t.value0;throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): "+[t.constructor.name])}};var ec={apply:function(t){return function(r){if(t instanceof F)return _(Re)(t.value0)(r);if(t instanceof W)return W.value;throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): "+[t.constructor.name,r.constructor.name])}},Functor0:function(){return Re}},wa={bind:function(t){return function(r){if(t instanceof F)return r(t.value0);if(t instanceof W)return W.value;throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): "+[t.constructor.name,r.constructor.name])}},Apply0:function(){return ec}};var Wo=function(){return{pure:F.create,Apply0:function(){return ec}}}();var Kt=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Yt=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var Nc={map:function(t){return function(r){if(r instanceof Kt)return new Kt(r.value0);if(r instanceof Yt)return new Yt(t(r.value0));throw new Error("Failed pattern match at Data.Either (line 31, column 1 - line 31, column 52): "+[r.constructor.name])}}};var ja=function(t){return function(r){return function(e){if(e instanceof Kt)return t(e.value0);if(e instanceof Yt)return r(e.value0);throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},Fp=function(){return ja(T(W.value))(F.create)}();var Su=function(t){return t};var so={map:function(t){return function(r){return t(r)}}};var Vy={apply:function(t){return function(r){return t(r)}},Functor0:function(){return so}},VS={bind:function(t){return function(r){return r(t)}},Apply0:function(){return Vy}},rv={pure:Su,Apply0:function(){return Vy}},zu={Applicative0:function(){return rv},Bind1:function(){return VS}};var Gy=function(t){return Math.min(Math.abs(t),2147483647)},Jy=function(t){return function(r){return r===0?0:r>0?Math.floor(t/r):-Math.floor(t/-r)}},jy=function(t){return function(r){if(r===0)return 0;var e=Math.abs(r);return(t%e+e)%e}},Xy=function(t){return function(r){return t/r}};var Qy={Ring0:function(){return Mc}},Ky={Ring0:function(){return Qm}};var ou=function(t){return t.mod};var zl={degree:function(t){return 1},div:Xy,mod:function(t){return function(r){return 0}},CommutativeRing0:function(){return Qy}},mo={degree:Gy,div:Jy,mod:jy,CommutativeRing0:function(){return Ky}},vo=function(t){return t.div};var Ne={mempty:void 0,Semigroup0:function(){return ky}},Tf={mempty:"",Semigroup0:function(){return Vm}};var Pt=function(t){return t.mempty},rn=function(t){return{mempty:function(r){return Pt(t)},Semigroup0:function(){return Gm(t.Semigroup0())}}};var ev=function(t){return function(){return t}},Yy=function(t){return function(r){return function(){return r(t())()}}};var Vl=function(t){return function(r){return function(){for(var e=0,n=t.length;e<n;e++)r(t[e])()}}};var tA=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}},qo={Applicative0:function(){return w},Bind1:function(){return Gn}},Gn={bind:Yy,Apply0:function(){return nv(0)}},w={pure:ev,Apply0:function(){return nv(0)}},rA=tA("functorEffect","Effect",function(){return{map:Bl(w)}}),nv=tA("applyEffect","Effect",function(){return{apply:_o(qo),Functor0:function(){return rA(0)}}}),S=rA(20),ot=nv(23),eA=function(t){return{append:Zn(ot)(bt(t))}},Le=function(t){return{mempty:ev(Pt(t)),Semigroup0:function(){return eA(t.Semigroup0())}}};var nA=function(t){return function(){return{value:t}}};var Ee=function(t){return function(){return t.value}},aA=function(t){return function(r){return function(){var e=t(r.value);return r.value=e.state,e.value}}},kn=function(t){return function(r){return function(){r.value=t}}};var ie=nA,QS=aA,Sf=function(t){return QS(function(r){var e=t(r);return{state:e,value:e}})},Lc=function(t){return function(r){return cr(S)(Sf(t)(r))}};var Ia=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Pa=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Zo=function(t){return t.tailRecM};var uA={tailRecM:function(t){return function(r){var e=function(n){if(n instanceof Pa)return n.value0;throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 113, column 30 - line 113, column 44): "+[n.constructor.name])};return function(){var a=Vn(Gn)(ie)(t(r))();return function(){for(;!function(){var i=Ee(a)();if(i instanceof Ia){var o=t(i.value0)();return kn(o)(a)(),!1}if(i instanceof Pa)return!0;throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 104, column 22 - line 109, column 28): "+[i.constructor.name])}(););return{}}(),_(S)(e)(Ee(a))()}}},Monad0:function(){return qo}};var oA=function(t){return function(r){return function(){return t(r())}}};function fn(t){return function(){return{value:t}}}var We=function(t){return function(){return t.value}},iA=function(t){return function(r){return function(){var e=t(r.value);return r.value=e.state,e.value}}},gn=function(t){return function(r){return function(){return r.value=t}}};var nx=iA,fu=function(t){return nx(function(r){var e=t(r);return{state:e,value:e}})},xu={map:oA};function Bc(){return[]}var uv=function(t){return function(r){return function(){return r.push.apply(r,t)}}};var Op=function(t){return function(r){return function(e){return function(n){return function(){return n.splice.apply(n,[t,r].concat(e))}}}}};function ax(t){return function(){return t.slice()}}var $p=ax;var ux=function(){function t(r,e,n,a,u,i){var o,p,s,f,m,v,c;for(o=u+(i-u>>1),o-u>1&&t(r,e,a,n,u,o),i-o>1&&t(r,e,a,n,o,i),p=u,s=o,f=u;p<o&&s<i;)m=a[p],v=a[s],c=e(r(m)(v)),c>0?(n[f++]=v,++s):(n[f++]=m,++p);for(;p<o;)n[f++]=a[p++];for(;s<i;)n[f++]=a[s++]}return function(r){return function(e){return function(n){return function(){return n.length<2||t(r,e,n,n.slice(0),0,n.length),n}}}}}();var nc=function(t){return uv([t])};var sA=function(t){return function(r){return t&&r}},mA=function(t){return function(r){return t||r}},vA=function(t){return!t};var lu=function(t){return t.not};var ac=function(t){return t.disj},Xa={ff:!1,tt:!0,implies:function(t){return function(r){return ac(Xa)(lu(Xa)(t))(r)}},conj:sA,disj:mA,not:vA};var dA=function(t){return function(r){return function(e){for(var n=r,a=e.length,u=a-1;u>=0;u--)n=t(e[u])(n);return n}}},bA=function(t){return function(r){return function(e){for(var n=r,a=e.length,u=0;u<a;u++)n=t(n)(e[u]);return n}}};var O=function(t){return t.empty};var et=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),Of=function(t){return function(r){return t(r.value0)(r.value1)}};var ln=function(t){return t.value1};var zo={map:function(t){return function(r){return new et(r.value0,t(r.value1))}}};var Qa=function(t){return t.value0};var Hc=function(t){return function(r){return function(e){return t(new et(r,e))}}};var mt=function(t){return t};var Jn=function(){return mt};var _n=Jn,Te=Jn;var _v=function(){return function(){return function(t){return Jn()}}};var ae=function(t){return t.foldr};var xe=function(t){return function(r){return ae(t)(I(r.Alt0()))(O(r))}},Cn=function(t){return function(r){return function(e){return ae(t)(function(){var n=I(r.Alt0());return function(a){return n(e(a))}}())(O(r))}}},ce=function(t){return function(r){return function(e){return ae(r)(function(){var n=Q(t.Apply0());return function(a){return n(e(a))}}())(l(t)(void 0))}}},jn=function(t){return function(r){return Mt(ce(t)(r))}},Ip=function(t){return function(r){return ce(t)(r)(rt(nt))}},be=function(t){return t.foldl};var te={foldr:function(t){return function(r){return function(e){if(e instanceof W)return r;if(e instanceof F)return t(e.value0)(r);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},foldl:function(t){return function(r){return function(e){if(e instanceof W)return r;if(e instanceof F)return t(r)(e.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},foldMap:function(t){return function(r){return function(e){if(e instanceof W)return Pt(t);if(e instanceof F)return r(e.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[r.constructor.name,e.constructor.name])}}}};var CA=function(t){return function(r){return function(e){return ae(t)(function(n){return function(a){return bt(r.Semigroup0())(e(n))(a)}})(Pt(r))}}},Bt={foldr:dA,foldl:bA,foldMap:function(t){return CA(Bt)(t)}};var hn=function(t){return t.foldMap};var hA=function(){function t(a){return[a]}function r(a){return function(u){return[a,u]}}function e(a){return function(u){return function(i){return[a,u,i]}}}function n(a){return function(u){return a.concat(u)}}return function(a){return function(u){return function(i){return function(o){return function(p){function s(f,m){switch(m-f){case 0:return i([]);case 1:return u(t)(o(p[f]));case 2:return a(u(r)(o(p[f])))(o(p[f+1]));case 3:return a(a(u(e)(o(p[f])))(o(p[f+1])))(o(p[f+2]));default:var v=f+Math.floor((m-f)/4)*2;return a(u(n)(s(f,v)))(s(v,m))}}return s(0,p.length)}}}}}}();var Nn=function(t){return t.traverse};var MA=function(t){return function(r){return Nn(t)(r)(rt(nt))}},bo={traverse:function(t){return hA(Wt(t.Apply0()))(_(t.Apply0().Functor0()))(l(t))},sequence:function(t){return MA(bo)(t)},Functor0:function(){return $r},Foldable1:function(){return Bt}};var a_=function(){return kf(et.create)}();var Ev=function(){return Sy};var BA=function(t){return[t]};var u_=function(){return hy(F.create)(W.value)}(),Tv=function(t){return u_(t)(Oa(t)-1|0)};var HA=function(){return Ey(F.create)(W.value)}();var Sv=function(){return Ty(F.create)(W.value)}(),qp=function(t){return function(r){return function(e){return e.length===0?[]:Jt(e)(function(n){return fa()(Sv(n)(e))})(HA(t(r))(e))}}};var ic=function(t){return function(r){return bt(cn)([t])(r)}};var UA=function(t){return function(r){for(var e=r.length,n=Array(e),a=0;a<e;a++)n[a]=t(a)(r[a]);return n}};var yo=function(t){return t.mapWithIndex};var ki={mapWithIndex:UA,Functor0:function(){return $r}};var Go=function(t){return t.foldrWithIndex};var Ju=function(t){return t.foldlWithIndex};var gi=function(t){return t.foldMapWithIndex};var cc=function(t){return t.traverseWithIndex};var ko=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}();var Zp=function(t){return function(r){return new ko(r,O(t))}};var we=function(){function t(){}return t.value=new t,t}(),pr=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),zp=function(t){return t},PF=function(t){return new pr(t.value0,t.value1)};var IF=function(t){var r=function(e){return function(n){var a=e,u=!1,i;function o(p,s){if(s instanceof pr&&s.value1 instanceof pr&&s.value1.value1 instanceof pr){a=new pr(s,p),n=s.value1.value1.value1;return}var f=function(v){return v instanceof pr&&v.value1 instanceof pr&&v.value1.value1 instanceof we?new pr(t(v.value0),new pr(t(v.value1.value0),we.value)):v instanceof pr&&v.value1 instanceof we?new pr(t(v.value0),we.value):we.value},m=function(v){return function(c){var h=v,ut=!1,dt;function fr(Zt,Kr){if(Zt instanceof pr&&Zt.value0 instanceof pr&&Zt.value0.value1 instanceof pr&&Zt.value0.value1.value1 instanceof pr){h=Zt.value1,c=new pr(t(Zt.value0.value0),new pr(t(Zt.value0.value1.value0),new pr(t(Zt.value0.value1.value1.value0),Kr)));return}return ut=!0,Kr}for(;!ut;)dt=fr(h,c);return dt}};return u=!0,m(p)(f(s))}for(;!u;)i=o(a,n);return i}};return r(we.value)},Vp={map:IF};var Ka={foldr:function(t){return function(r){var e=function(){var a=function(u){return function(i){var o=u,p=!1,s;function f(m,v){if(v instanceof we)return p=!0,m;if(v instanceof pr){o=new pr(v.value0,m),i=v.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): "+[m.constructor.name,v.constructor.name])}for(;!p;)s=f(o,i);return s}};return a(we.value)}(),n=be(Ka)(Mt(t))(r);return function(a){return n(e(a))}}},foldl:function(t){var r=function(e){return function(n){var a=e,u=!1,i;function o(p,s){if(s instanceof we)return u=!0,p;if(s instanceof pr){a=t(p)(s.value0),n=s.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): "+[s.constructor.name])}for(;!u;)i=o(a,n);return i}};return r},foldMap:function(t){return function(r){return be(Ka)(function(e){var n=bt(t.Semigroup0())(e);return function(a){return n(r(a))}})(Pt(t))}}};var o_={append:function(t){return function(r){return ae(Ka)(pr.create)(r)(t)}}};var Fv={append:function(t){return function(r){return new ko(t.value0,bt(o_)(t.value1)(PF(r)))}}};var qA={alt:bt(o_),Functor0:function(){return Vp}},Ov=function(){return{empty:we.value,Alt0:function(){return qA}}}();var jA=function(t){return t()};var XA=function(t){throw new Error(t)};var QA=function(){return XA};var eO=jA,_u=function(t){return eO(function(){return QA()(t)})};var tr=function(){function t(){}return t.value=new t,t}(),dr=function(){function t(r,e,n,a){this.value0=r,this.value1=e,this.value2=n,this.value3=a}return t.create=function(r){return function(e){return function(n){return function(a){return new t(r,e,n,a)}}}},t}(),Ur=function(){function t(r,e,n,a,u,i,o){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=i,this.value6=o}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return function(o){return new t(r,e,n,a,u,i,o)}}}}}}},t}(),lc=function(){function t(r,e,n){this.value0=r,this.value1=e,this.value2=n}return t.create=function(r){return function(e){return function(n){return new t(r,e,n)}}},t}(),Ei=function(){function t(r,e,n){this.value0=r,this.value1=e,this.value2=n}return t.create=function(r){return function(e){return function(n){return new t(r,e,n)}}},t}(),_c=function(){function t(r,e,n,a,u,i){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=i}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return new t(r,e,n,a,u,i)}}}}}},t}(),jo=function(){function t(r,e,n,a,u,i){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=i}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return new t(r,e,n,a,u,i)}}}}}},t}(),pc=function(){function t(r,e,n,a,u,i){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=i}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return new t(r,e,n,a,u,i)}}}}}},t}(),Jp=function(){function t(r,e,n,a){this.value0=r,this.value1=e,this.value2=n,this.value3=a}return t.create=function(r){return function(e){return function(n){return function(a){return new t(r,e,n,a)}}}},t}();var YA=function(t){return function(r){return new dr(tr.value,t,r,tr.value)}};var Xp=function(t){return function(r){var e=ur(t),n=function(a){var u=!1,i;function o(p){if(p instanceof tr)return u=!0,W.value;if(p instanceof dr){var s=e(r)(p.value1);if(s instanceof kr)return u=!0,new F(p.value2);if(s instanceof ar){a=p.value0;return}a=p.value3;return}if(p instanceof Ur){var f=e(r)(p.value1);if(f instanceof kr)return u=!0,new F(p.value2);var m=e(r)(p.value4);if(m instanceof kr)return u=!0,new F(p.value5);if(f instanceof ar){a=p.value0;return}if(m instanceof Ar){a=p.value6;return}a=p.value3;return}throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): "+[p.constructor.name])}for(;!u;)i=o(a);return i};return n}};var tk=function(t){return t instanceof tr};var sn=function(t){return function(r){return function(e){var n=t,a=r,u=!1,i;function o(p,s,f){if(s instanceof we)return u=!0,f;if(s instanceof pr){if(s.value0 instanceof lc){n=p,a=s.value1,e=new dr(f,s.value0.value0,s.value0.value1,s.value0.value2);return}if(s.value0 instanceof Ei){n=p,a=s.value1,e=new dr(s.value0.value0,s.value0.value1,s.value0.value2,f);return}if(s.value0 instanceof _c){n=p,a=s.value1,e=new Ur(f,s.value0.value0,s.value0.value1,s.value0.value2,s.value0.value3,s.value0.value4,s.value0.value5);return}if(s.value0 instanceof jo){n=p,a=s.value1,e=new Ur(s.value0.value0,s.value0.value1,s.value0.value2,f,s.value0.value3,s.value0.value4,s.value0.value5);return}if(s.value0 instanceof pc){n=p,a=s.value1,e=new Ur(s.value0.value0,s.value0.value1,s.value0.value2,s.value0.value3,s.value0.value4,s.value0.value5,f);return}throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): "+[s.value0.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): "+[s.constructor.name,f.constructor.name])}for(;!u;)i=o(n,a,e);return i}}},c_=function(t){return function(r){return function(e){var n=function(i){return function(o){var p=i,s=!1,f;function m(v,c){if(v instanceof we)return s=!0,new dr(c.value0,c.value1,c.value2,c.value3);if(v instanceof pr){if(v.value0 instanceof lc)return s=!0,sn(t)(v.value1)(new Ur(c.value0,c.value1,c.value2,c.value3,v.value0.value0,v.value0.value1,v.value0.value2));if(v.value0 instanceof Ei)return s=!0,sn(t)(v.value1)(new Ur(v.value0.value0,v.value0.value1,v.value0.value2,c.value0,c.value1,c.value2,c.value3));if(v.value0 instanceof _c){p=v.value1,o=new Jp(new dr(c.value0,c.value1,c.value2,c.value3),v.value0.value0,v.value0.value1,new dr(v.value0.value2,v.value0.value3,v.value0.value4,v.value0.value5));return}if(v.value0 instanceof jo){p=v.value1,o=new Jp(new dr(v.value0.value0,v.value0.value1,v.value0.value2,c.value0),c.value1,c.value2,new dr(c.value3,v.value0.value3,v.value0.value4,v.value0.value5));return}if(v.value0 instanceof pc){p=v.value1,o=new Jp(new dr(v.value0.value0,v.value0.value1,v.value0.value2,v.value0.value3),v.value0.value4,v.value0.value5,new dr(c.value0,c.value1,c.value2,c.value3));return}throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): "+[v.value0.constructor.name,c.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): "+[v.constructor.name,c.constructor.name])}for(;!s;)f=m(p,o);return f}},a=ur(t),u=function(i){return function(o){var p=i,s=!1,f;function m(v,c){if(c instanceof tr)return s=!0,n(v)(new Jp(tr.value,r,e,tr.value));if(c instanceof dr){var h=a(r)(c.value1);if(h instanceof kr)return s=!0,sn(t)(v)(new dr(c.value0,r,e,c.value3));if(h instanceof ar){p=new pr(new lc(c.value1,c.value2,c.value3),v),o=c.value0;return}p=new pr(new Ei(c.value0,c.value1,c.value2),v),o=c.value3;return}if(c instanceof Ur){var ut=a(r)(c.value1);if(ut instanceof kr)return s=!0,sn(t)(v)(new Ur(c.value0,r,e,c.value3,c.value4,c.value5,c.value6));var dt=a(r)(c.value4);if(dt instanceof kr)return s=!0,sn(t)(v)(new Ur(c.value0,c.value1,c.value2,c.value3,r,e,c.value6));if(ut instanceof ar){p=new pr(new _c(c.value1,c.value2,c.value3,c.value4,c.value5,c.value6),v),o=c.value0;return}if(ut instanceof Ar&&dt instanceof ar){p=new pr(new jo(c.value0,c.value1,c.value2,c.value4,c.value5,c.value6),v),o=c.value3;return}p=new pr(new pc(c.value0,c.value1,c.value2,c.value3,c.value4,c.value5),v),o=c.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): "+[v.constructor.name,c.constructor.name])}for(;!s;)f=m(p,o);return f}};return u(we.value)}}},cO=function(t){return function(r){var e=function(o){return function(p){var s=o,f=!1,m;function v(c,h){if(c instanceof we)return f=!0,h;if(c instanceof pr){if(c.value0 instanceof lc&&c.value0.value2 instanceof tr&&h instanceof tr)return f=!0,sn(t)(c.value1)(new dr(tr.value,c.value0.value0,c.value0.value1,tr.value));if(c.value0 instanceof Ei&&c.value0.value0 instanceof tr&&h instanceof tr)return f=!0,sn(t)(c.value1)(new dr(tr.value,c.value0.value1,c.value0.value2,tr.value));if(c.value0 instanceof lc&&c.value0.value2 instanceof dr){s=c.value1,p=new Ur(h,c.value0.value0,c.value0.value1,c.value0.value2.value0,c.value0.value2.value1,c.value0.value2.value2,c.value0.value2.value3);return}if(c.value0 instanceof Ei&&c.value0.value0 instanceof dr){s=c.value1,p=new Ur(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3,c.value0.value1,c.value0.value2,h);return}return c.value0 instanceof lc&&c.value0.value2 instanceof Ur?(f=!0,sn(t)(c.value1)(new dr(new dr(h,c.value0.value0,c.value0.value1,c.value0.value2.value0),c.value0.value2.value1,c.value0.value2.value2,new dr(c.value0.value2.value3,c.value0.value2.value4,c.value0.value2.value5,c.value0.value2.value6)))):c.value0 instanceof Ei&&c.value0.value0 instanceof Ur?(f=!0,sn(t)(c.value1)(new dr(new dr(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3),c.value0.value0.value4,c.value0.value0.value5,new dr(c.value0.value0.value6,c.value0.value1,c.value0.value2,h)))):c.value0 instanceof _c&&c.value0.value2 instanceof tr&&c.value0.value5 instanceof tr&&h instanceof tr?(f=!0,sn(t)(c.value1)(new Ur(tr.value,c.value0.value0,c.value0.value1,tr.value,c.value0.value3,c.value0.value4,tr.value))):c.value0 instanceof jo&&c.value0.value0 instanceof tr&&c.value0.value5 instanceof tr&&h instanceof tr?(f=!0,sn(t)(c.value1)(new Ur(tr.value,c.value0.value1,c.value0.value2,tr.value,c.value0.value3,c.value0.value4,tr.value))):c.value0 instanceof pc&&c.value0.value0 instanceof tr&&c.value0.value3 instanceof tr&&h instanceof tr?(f=!0,sn(t)(c.value1)(new Ur(tr.value,c.value0.value1,c.value0.value2,tr.value,c.value0.value4,c.value0.value5,tr.value))):c.value0 instanceof _c&&c.value0.value2 instanceof dr?(f=!0,sn(t)(c.value1)(new dr(new Ur(h,c.value0.value0,c.value0.value1,c.value0.value2.value0,c.value0.value2.value1,c.value0.value2.value2,c.value0.value2.value3),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof jo&&c.value0.value0 instanceof dr?(f=!0,sn(t)(c.value1)(new dr(new Ur(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3,c.value0.value1,c.value0.value2,h),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof jo&&c.value0.value5 instanceof dr?(f=!0,sn(t)(c.value1)(new dr(c.value0.value0,c.value0.value1,c.value0.value2,new Ur(h,c.value0.value3,c.value0.value4,c.value0.value5.value0,c.value0.value5.value1,c.value0.value5.value2,c.value0.value5.value3)))):c.value0 instanceof pc&&c.value0.value3 instanceof dr?(f=!0,sn(t)(c.value1)(new dr(c.value0.value0,c.value0.value1,c.value0.value2,new Ur(c.value0.value3.value0,c.value0.value3.value1,c.value0.value3.value2,c.value0.value3.value3,c.value0.value4,c.value0.value5,h)))):c.value0 instanceof _c&&c.value0.value2 instanceof Ur?(f=!0,sn(t)(c.value1)(new Ur(new dr(h,c.value0.value0,c.value0.value1,c.value0.value2.value0),c.value0.value2.value1,c.value0.value2.value2,new dr(c.value0.value2.value3,c.value0.value2.value4,c.value0.value2.value5,c.value0.value2.value6),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof jo&&c.value0.value0 instanceof Ur?(f=!0,sn(t)(c.value1)(new Ur(new dr(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3),c.value0.value0.value4,c.value0.value0.value5,new dr(c.value0.value0.value6,c.value0.value1,c.value0.value2,h),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof jo&&c.value0.value5 instanceof Ur?(f=!0,sn(t)(c.value1)(new Ur(c.value0.value0,c.value0.value1,c.value0.value2,new dr(h,c.value0.value3,c.value0.value4,c.value0.value5.value0),c.value0.value5.value1,c.value0.value5.value2,new dr(c.value0.value5.value3,c.value0.value5.value4,c.value0.value5.value5,c.value0.value5.value6)))):c.value0 instanceof pc&&c.value0.value3 instanceof Ur?(f=!0,sn(t)(c.value1)(new Ur(c.value0.value0,c.value0.value1,c.value0.value2,new dr(c.value0.value3.value0,c.value0.value3.value1,c.value0.value3.value2,c.value0.value3.value3),c.value0.value3.value4,c.value0.value3.value5,new dr(c.value0.value3.value6,c.value0.value4,c.value0.value5,h)))):(f=!0,_u("The impossible happened in partial function `up`."))}throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): "+[c.constructor.name])}for(;!f;)m=v(s,p);return m}},n=function(o){return function(p){var s=o,f=!1,m;function v(c,h){if(h instanceof dr&&h.value0 instanceof tr&&h.value3 instanceof tr)return f=!0,e(c)(tr.value);if(h instanceof dr){s=new pr(new Ei(h.value0,h.value1,h.value2),c),p=h.value3;return}if(h instanceof Ur&&h.value0 instanceof tr&&h.value3 instanceof tr&&h.value6 instanceof tr)return f=!0,e(new pr(new Ei(tr.value,h.value1,h.value2),c))(tr.value);if(h instanceof Ur){s=new pr(new pc(h.value0,h.value1,h.value2,h.value3,h.value4,h.value5),c),p=h.value6;return}return f=!0,_u("The impossible happened in partial function `removeMaxNode`.")}for(;!f;)m=v(s,p);return m}},a=function(o){var p=!1,s;function f(m){if(m instanceof dr&&m.value3 instanceof tr)return p=!0,{key:m.value1,value:m.value2};if(m instanceof dr){o=m.value3;return}if(m instanceof Ur&&m.value6 instanceof tr)return p=!0,{key:m.value4,value:m.value5};if(m instanceof Ur){o=m.value6;return}return p=!0,_u("The impossible happened in partial function `maxNode`.")}for(;!p;)s=f(o);return s},u=ur(t),i=function(o){return function(p){var s=o,f=!1,m;function v(c,h){if(h instanceof tr)return f=!0,W.value;if(h instanceof dr){var ut=u(r)(h.value1);if(h.value3 instanceof tr&&ut instanceof kr)return f=!0,new F(new et(h.value2,e(c)(tr.value)));if(ut instanceof kr){var dt=a(h.value0);return f=!0,new F(new et(h.value2,n(new pr(new lc(dt.key,dt.value,h.value3),c))(h.value0)))}if(ut instanceof ar){s=new pr(new lc(h.value1,h.value2,h.value3),c),p=h.value0;return}s=new pr(new Ei(h.value0,h.value1,h.value2),c),p=h.value3;return}if(h instanceof Ur){var fr=function(){return h.value0 instanceof tr&&h.value3 instanceof tr&&h.value6 instanceof tr}(),ut=u(r)(h.value4),Zt=u(r)(h.value1);if(fr&&Zt instanceof kr)return f=!0,new F(new et(h.value2,sn(t)(c)(new dr(tr.value,h.value4,h.value5,tr.value))));if(fr&&ut instanceof kr)return f=!0,new F(new et(h.value5,sn(t)(c)(new dr(tr.value,h.value1,h.value2,tr.value))));if(Zt instanceof kr){var dt=a(h.value0);return f=!0,new F(new et(h.value2,n(new pr(new _c(dt.key,dt.value,h.value3,h.value4,h.value5,h.value6),c))(h.value0)))}if(ut instanceof kr){var dt=a(h.value3);return f=!0,new F(new et(h.value5,n(new pr(new jo(h.value0,h.value1,h.value2,dt.key,dt.value,h.value6),c))(h.value3)))}if(Zt instanceof ar){s=new pr(new _c(h.value1,h.value2,h.value3,h.value4,h.value5,h.value6),c),p=h.value0;return}if(Zt instanceof Ar&&ut instanceof ar){s=new pr(new jo(h.value0,h.value1,h.value2,h.value4,h.value5,h.value6),c),p=h.value3;return}s=new pr(new pc(h.value0,h.value1,h.value2,h.value3,h.value4,h.value5),c),p=h.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): "+[h.constructor.name])}for(;!f;)m=v(s,p);return m}};return i(we.value)}},Na={foldr:function(t){return function(r){return function(e){if(e instanceof tr)return r;if(e instanceof dr)return ae(Na)(t)(t(e.value2)(ae(Na)(t)(r)(e.value3)))(e.value0);if(e instanceof Ur)return ae(Na)(t)(t(e.value2)(ae(Na)(t)(t(e.value5)(ae(Na)(t)(r)(e.value6)))(e.value3)))(e.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): "+[e.constructor.name])}}},foldl:function(t){return function(r){return function(e){if(e instanceof tr)return r;if(e instanceof dr)return be(Na)(t)(t(be(Na)(t)(r)(e.value0))(e.value2))(e.value3);if(e instanceof Ur)return be(Na)(t)(t(be(Na)(t)(t(be(Na)(t)(r)(e.value0))(e.value2))(e.value3))(e.value5))(e.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): "+[e.constructor.name])}}},foldMap:function(t){return function(r){return function(e){if(e instanceof tr)return Pt(t);if(e instanceof dr)return bt(t.Semigroup0())(hn(Na)(t)(r)(e.value0))(bt(t.Semigroup0())(r(e.value2))(hn(Na)(t)(r)(e.value3)));if(e instanceof Ur)return bt(t.Semigroup0())(hn(Na)(t)(r)(e.value0))(bt(t.Semigroup0())(r(e.value2))(bt(t.Semigroup0())(hn(Na)(t)(r)(e.value3))(bt(t.Semigroup0())(r(e.value5))(hn(Na)(t)(r)(e.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): "+[e.constructor.name])}}}},_a={foldrWithIndex:function(t){return function(r){return function(e){if(e instanceof tr)return r;if(e instanceof dr)return Go(_a)(t)(t(e.value1)(e.value2)(Go(_a)(t)(r)(e.value3)))(e.value0);if(e instanceof Ur)return Go(_a)(t)(t(e.value1)(e.value2)(Go(_a)(t)(t(e.value4)(e.value5)(Go(_a)(t)(r)(e.value6)))(e.value3)))(e.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 147, column 26 - line 150, column 120): "+[e.constructor.name])}}},foldlWithIndex:function(t){return function(r){return function(e){if(e instanceof tr)return r;if(e instanceof dr)return Ju(_a)(t)(t(e.value1)(Ju(_a)(t)(r)(e.value0))(e.value2))(e.value3);if(e instanceof Ur)return Ju(_a)(t)(t(e.value4)(Ju(_a)(t)(t(e.value1)(Ju(_a)(t)(r)(e.value0))(e.value2))(e.value3))(e.value5))(e.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 151, column 26 - line 154, column 120): "+[e.constructor.name])}}},foldMapWithIndex:function(t){return function(r){return function(e){if(e instanceof tr)return Pt(t);if(e instanceof dr)return bt(t.Semigroup0())(gi(_a)(t)(r)(e.value0))(bt(t.Semigroup0())(r(e.value1)(e.value2))(gi(_a)(t)(r)(e.value3)));if(e instanceof Ur)return bt(t.Semigroup0())(gi(_a)(t)(r)(e.value0))(bt(t.Semigroup0())(r(e.value1)(e.value2))(bt(t.Semigroup0())(gi(_a)(t)(r)(e.value3))(bt(t.Semigroup0())(r(e.value4)(e.value5))(gi(_a)(t)(r)(e.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 155, column 26 - line 158, column 128): "+[e.constructor.name])}}},Foldable0:function(){return Na}},rk=function(){return Go(_a)(function(t){return function(r){return function(e){return new pr(t,e)}}})(we.value)}();var sc=function(){return tr.value}();var Iv=function(t){return function(r){return function(e){return Jt(e)(ln)(cO(t)(r)(e))}}};var mc=function(t){return function(r){return function(e){return function(n){var a=r(Xp(t)(e)(n));if(a instanceof W)return Iv(t)(e)(n);if(a instanceof F)return c_(t)(e)(a.value0)(n);throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): "+[a.constructor.name])}}}};var fO=function(t){return function(r){return function(e){return function(n){var a=function(u){return function(i){return function(o){return mc(t)(function(){var p=Jt(o)(r(o));return function(s){return F.create(p(s))}}())(u)(i)}}};return Ju(_a)(a)(n)(e)}}}};var ek=function(t){return fO(t)(T)};var f_=function(t){return t.partitionMap};var Ti=function(t){return t.filterMap};var l_=function(t){return t.filter};var mO=function(t){return t},__=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),p_=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),vO=function(t){return t},Yp=Jn(),b=vO;var q=function(){return __.create}();var st=function(){return p_.create}(),ne=function(){var t=_(qn)(_(S)(T(!0)));return function(r){return mO(t(r))}}(),tt=function(t){return t.attr};function ok(t){return()=>t.slice()}function ik(t){return r=>e=>()=>{e[t]=r}}function ck(t){return()=>t.slice()}var ts=mt;var G={liftST:ts,Monad0:function(){return qo}},St=function(t){return t.liftST};var en={dimap:function(t){return function(r){return function(e){return function(n){return r(e(t(n)))}}}}},go=function(t){return t.dimap},ka=function(t){return function(r){return go(t)(r)(rt(nt))}};var yO=function(t){return function(r){return function(e){return ek(t)(r)(e)}}};var Rv=function(t){return rk(t)};var pk=function(t){return YA(t)(void 0)};var Nv=function(t){return{append:yO(t)}};var sk=function(t){return tk(t)},mk=function(t){return function(r){return function(e){return c_(t)(r)(void 0)(e)}}};var vk={foldMap:function(t){return function(r){var e=hn(Ka)(t)(r);return function(n){return e(Rv(n))}}},foldl:function(t){return function(r){var e=be(Ka)(t)(r);return function(n){return e(Rv(n))}}},foldr:function(t){return function(r){var e=ae(Ka)(t)(r);return function(n){return e(Rv(n))}}}};var Lv=sc;var Dk=function(t){return{mempty:Lv,Semigroup0:function(){return Nv(t)}}};var rs=function(t){return function(r){return function(e){return Iv(t)(r)(e)}}};function dk(t){return function(r){return function(){return setTimeout(r,t)}}}function bk(t){return function(){clearTimeout(t)}}var es=dk;var kO={eq:function(t){return function(r){return t===r}}},ns={compare:function(t){return function(r){return ur(tn)(t)(r)}},Eq0:function(){return kO}};var m_=bk;var Si=function(t){return t.sampleOn};var Ce=function(t){return t.keepLatest};var ju=function(t){return t.fold};var v_=function(t){return function(r){return function(e){return function(n){return Ti(t.Filterable1())(ln)(ju(t)(function(a){return function(u){return _(zo)(l(Wo))(r(a)(u.value0))}})(e)(new et(n,W.value)))}}}},as=function(t){return function(r){var e=function(n){return function(a){if(a instanceof W)return new F({now:n,last:W.value});if(a instanceof F)return new F({now:n,last:new F(a.value0.now)});throw new Error("Failed pattern match at FRP.Event.Class (line 54, column 3 - line 54, column 50): "+[n.constructor.name,a.constructor.name])}};return Ti(t.Filterable1())(rt(nt))(ju(t)(e)(r)(W.value))}},D_=function(t){return t.fix};var Ln=function(t){return function(r){return function(e){return Wt(t.Alternative0().Applicative0().Apply0())(_(t.Filterable1().Functor1())(Yi)(r))(e)}}};var yk=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Ak=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),kk=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),gk=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),ft={map:function(t){return function(r){return function(e,n,a,u){return r(e,n,a,function(i){return u(t(i))})}}}},zc={apply:function(t){return function(r){return function(e,n,a,u){return a(function(i){return t(e,n,a,function(o){return a(function(p){return r(e,n,a,function(s){return u(o(s))})})})})}}},Functor0:function(){return ft}},Co={bind:function(t){return function(r){return function(e,n,a,u){return a(function(i){return t(e,n,a,function(o){var p=r(o);return a(function(s){return p(e,n,a,function(f){return u(f)})})})})}}},Apply0:function(){return zc}};var D={pure:function(t){return function(r,e,n,a){return a(t)}},Apply0:function(){return zc}},gO={Applicative0:function(){return D},Bind1:function(){return Co}};var _t=function(t){var r=function(e){var n=!1,a;function u(i){var o=i(void 0);if(o instanceof yk){e=o.value0;return}if(o instanceof Ak)return n=!0,_(S)(Ia.create)(o.value1);if(o instanceof kk)return n=!0,_(S)(Ia.create)(ts(o.value0));if(o instanceof gk)return n=!0,l(w)(new Pa(o.value0));throw new Error("Failed pattern match at Hyrule.Zora (line 131, column 15 - line 135, column 30): "+[o.constructor.name])}for(;!n;)a=u(e);return a};return Zo(uA)(r)(function(e){return t(function(n,a){return new Ak(n,a)},kk.create,yk.create,gk.create)})},Hv=function(t){return function(r,e,n,a){return e(_(xu)(function(u){return function(i){return a(u)}})(t))}},V={liftST:Hv,Monad0:function(){return gO}};var us=function(t){return function(r){return function(e,n,a,u){return e(function(i){return u(Pt(t))},_(S)(function(i){return function(o){return u(i)}})(r))}}};function Uv(t){return function(r){return t===r}}var os=Uv;var TO=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}},SO=function(t){return t};var pu=function(t){return go(en)(_(qn)(us(Ne)))(function(){var r=_(ft)(_t);return function(e){return _t(r(e))}}())(t)},xO=function(t){return function(r){return function(e){return function(n){return function(a){return P(t.Monad0().Bind1())(St(t)(fn(W.value)))(function(u){return P(t.Monad0().Bind1())(e(function(i){return St(t)(cr(xu)(gn(new F(i))(u)))}))(function(i){return P(t.Monad0().Bind1())(n(function(o){return P(t.Monad0().Bind1())(St(t)(We(u)))(ce(r)(te)(function(p){return a(o(p))}))}))(function(o){return l(r)(Q(r.Apply0())(i)(o))})})})}}}}},g={map:function(t){return function(r){return function(e){return r(function(n){return e(t(n))})}}}};var FO=function(t){return function(r){return function(e){return function(n){return function(a){return P(t.Monad0().Bind1())(St(t)(fn(n)))(function(u){return e(function(i){return P(t.Monad0().Bind1())(St(t)(fu(r(i))(u)))(a)})})}}}}},d_=function(t){return function(r){return function(e){return function(n){return e(function(a){var u=r(a);if(u instanceof F)return n(u.value0);if(u instanceof W)return l(t)(void 0);throw new Error("Failed pattern match at FRP.Event (line 189, column 13 - line 191, column 27): "+[u.constructor.name])})}}}},Wv=function(t){return function(r){return d_(t)(function(e){var n=r(e);if(n)return new F(e);if(!n)return W.value;throw new Error("Failed pattern match at FRP.Event (line 117, column 13 - line 119, column 25): "+[n.constructor.name])})}},hk=function(t){return{compact:d_(t)(rt(nt)),separate:function(r){return{left:d_(t)(function(e){if(e instanceof Kt)return new F(e.value0);if(e instanceof Yt)return W.value;throw new Error("Failed pattern match at FRP.Event (line 100, column 13 - line 102, column 33): "+[e.constructor.name])})(r),right:d_(t)(function(e){if(e instanceof Yt)return new F(e.value0);if(e instanceof Kt)return W.value;throw new Error("Failed pattern match at FRP.Event (line 107, column 13 - line 109, column 32): "+[e.constructor.name])})(r)}}}},Ou=function(t){return{filter:Wv(t),filterMap:d_(t),partition:function(r){return function(e){return{yes:Wv(t)(r)(e),no:Wv(t)(function(){var n=lu(Xa);return function(a){return n(r(a))}}())(e)}}},partitionMap:function(r){return function(e){return{left:Ti(Ou(t))(function(){var n=ja(F.create)(T(W.value));return function(a){return n(r(a))}}())(e),right:Ti(Ou(t))(function(n){return Fp(r(n))})(e)}}},Compactable0:function(){return hk(t)},Functor1:function(){return g}}},OO=function(t){return function(r){return function(e){return function(n){return function(a){return P(t.Monad0().Bind1())(St(t)(fn(W.value)))(function(u){return P(t.Monad0().Bind1())(St(t)(Bc))(function(i){return P(t.Monad0().Bind1())(St(t)(fn(W.value)))(function(o){return P(t.Monad0().Bind1())(St(t)(Bc))(function(p){return P(t.Monad0().Bind1())(St(t)(fn(!0)))(function(s){return P(t.Monad0().Bind1())(e(function(f){return P(t.Monad0().Bind1())(St(t)(We(s)))(function(m){return m?St(t)(cr(xu)(nc(f)(i))):P(t.Monad0().Bind1())(St(t)(gn(new F(f))(u)))(function(){return P(t.Monad0().Bind1())(St(t)(We(o)))(ce(r)(te)(function(v){return a(v(f))}))})})}))(function(f){return P(t.Monad0().Bind1())(n(function(m){return P(t.Monad0().Bind1())(St(t)(We(s)))(function(v){return v?St(t)(cr(xu)(nc(m)(p))):P(t.Monad0().Bind1())(St(t)(gn(new F(m))(o)))(function(){return P(t.Monad0().Bind1())(St(t)(We(u)))(ce(r)(te)(function(c){return a(m(c))}))})})}))(function(m){return P(t.Monad0().Bind1())(St(t)(gn(!1)(s)))(function(){return P(t.Monad0().Bind1())(St(t)($p(i)))(function(v){return P(t.Monad0().Bind1())(St(t)($p(p)))(function(c){return Xr(Qr)(t.Monad0().Bind1())(function(){return v.length===0?cr(r.Apply0().Functor0())(St(t)(gn(Tv(c))(o))):jn(r)(Bt)(v)(function(h){return P(t.Monad0().Bind1())(St(t)(gn(new F(h))(u)))(function(){return jn(r)(Bt)(c)(function(ut){return P(t.Monad0().Bind1())(St(t)(gn(new F(ut))(o)))(function(){return a(ut(h))})})})})}())(function(){return P(t.Monad0().Bind1())(St(t)(Op(0)(Oa(v))([])(i)))(function(){return P(t.Monad0().Bind1())(St(t)(Op(0)(Oa(c))([])(p)))(function(){return l(r)(Q(r.Apply0())(f)(m))})})})})})})})})})})})})})}}}}},$t=function(t){return function(r){return r}(is(277).subscribe)(t)},Ut=function(t){return function(r){return r}(is(300).makeEvent)(t)},Lf=function(t){return function(r){return Xr(Qr)(t.Monad0().Bind1())(l(t.Monad0().Applicative0())(void 0))(function(){return function(e){return e(t)(r)}(is(316).create)})}},is=TO("backdoor","FRP.Event",function(){return{makeEvent:SO,create:function(){var t=function(r){return function(e){return P(r.Monad0().Bind1())(St(r)(fn([])))(function(n){return l(r.Monad0().Applicative0())({event:function(a){return P(e.Monad0().Bind1())(St(e)(fu(function(u){return bt(cn)(u)([a])})(n)))(function(){return l(e.Monad0().Applicative0())(P(e.Monad0().Bind1())(St(e)(fu(qp(os)(a))(n)))(function(){return l(e.Monad0().Applicative0())(void 0)}))})},push:function(a){return P(e.Monad0().Bind1())(St(e)(We(n)))(ce(e.Monad0().Applicative0())(Bt)(function(u){return u(a)}))}})})}};return t}(),subscribe:function(){var t=function(r){return function(e){return r(e)}};return t}(),bus:function(){var t=function(r){return function(e){return Ut(function(n){return P(r.Monad0().Bind1())(Lf(r)(r))(function(a){return Xr(Qr)(r.Monad0().Bind1())(n(e(a.push)(a.event)))(function(){return l(r.Monad0().Applicative0())(l(r.Monad0().Applicative0())(void 0))})})})}};return t}(),memoize:function(){var t=function(r){return function(e){return function(n){return Ut(function(a){return P(r.Monad0().Bind1())(Lf(r)(r))(function(u){return Xr(Qr)(r.Monad0().Bind1())(a(n(u.event)))(function(){return $t(e)(u.push)})})})}}};return t}(),hot:function(){var t=function(r){return function(e){return P(r.Monad0().Bind1())(Lf(r)(r))(function(n){return P(r.Monad0().Bind1())($t(e)(n.push))(function(a){return l(r.Monad0().Applicative0())({event:n.event,unsubscribe:a})})})}};return t}(),mailboxed:function(){var t=function(r){return function(e){return function(n){return function(a){return Ut(function(u){return P(e.Monad0().Bind1())(St(e)(fn(sc)))(function(i){return Xr(Qr)(e.Monad0().Bind1())(u(a(function(o){return Ut(function(p){return Xr(Qr)(e.Monad0().Bind1())(cr(e.Monad0().Bind1().Apply0().Functor0())(St(e)(fu(mc(r)(function(s){if(s instanceof W)return new F([p]);if(s instanceof F)return new F(bt(cn)(s.value0)([p]));throw new Error("Failed pattern match at FRP.Event (line 484, column 21 - line 486, column 55): "+[s.constructor.name])})(o))(i))))(function(){return l(e.Monad0().Applicative0())(cr(e.Monad0().Bind1().Apply0().Functor0())(St(e)(fu(mc(r)(function(s){if(s instanceof W)return W.value;if(s instanceof F)return new F(qp(os)(p)(s.value0));throw new Error("Failed pattern match at FRP.Event (line 493, column 21 - line 495, column 69): "+[s.constructor.name])})(o))(i))))})})})))(function(){return P(e.Monad0().Bind1())($t(n)(function(o){return P(e.Monad0().Bind1())(St(e)(We(i)))(function(p){var s=Xp(r)(o.address)(p);if(s instanceof W)return l(e.Monad0().Applicative0())(void 0);if(s instanceof F)return jn(e.Monad0().Applicative0())(Bt)(s.value0)(function(f){return f(o.payload)});throw new Error("Failed pattern match at FRP.Event (line 502, column 13 - line 504, column 49): "+[s.constructor.name])})}))(function(o){return l(e.Monad0().Applicative0())(Xr(Qr)(e.Monad0().Bind1())(cr(e.Monad0().Bind1().Apply0().Functor0())(St(e)(gn(sc)(i))))(function(){return o}))})})})})}}}};return t}(),delay:function(){var t=function(r){return function(e){return Ut(function(n){return function(){var u=ie(Pt(Dk(ns)))(),i=$t(e)(function(o){return function(){var s=ie(W.value)(),f=es(r)(function(){n(o)();var v=Ee(s)();return Jt(l(w)(void 0))(function(c){return Lc(rs(ns)(c))(u)})(v)()})();return kn(new F(f))(s)(),Lc(bt(Nv(ns))(pk(f)))(u)()}})();return function(){var p=Ee(u)();return jn(w)(vk)(p)(m_)(),i()}}})}};return t}()}}),qv=is(417),Zv=function(t){return function(r){return function(e){return e(t)}(qv.bus)(r)}},Xu=function(t){return function(r){return r}(qv.delay)(t)};var Xo=function(t){return function(r){return function(e){return e(t)}(qv.memoize)(r)}},$O=function(t){return function(r){return function(e){return function(n){return P(r.Bind1())(Lf(t)(t))(function(a){var u=e(a.event);return P(r.Bind1())($t(u.input)(a.push))(function(i){return P(r.Bind1())($t(u.output)(n))(function(o){return l(r.Applicative0())(Q(r.Bind1().Apply0())(i)(o))})})})}}}},wO=function(t){return function(r){return function(e){return P(t.Monad0().Bind1())(St(t)(fn(W.value)))(function(n){return P(t.Monad0().Bind1())(r(function(a){return Xr(Qr)(t.Monad0().Bind1())(P(t.Monad0().Bind1())(St(t)(We(n)))(Ip(t.Monad0().Applicative0())(te)))(function(){return P(t.Monad0().Bind1())($t(a)(e))(function(u){return St(t)(cr(xu)(gn(new F(u))(n)))})})}))(function(a){return l(t.Monad0().Applicative0())(Xr(Qr)(t.Monad0().Bind1())(P(t.Monad0().Bind1())(St(t)(We(n)))(Ip(t.Monad0().Applicative0())(te)))(function(){return a}))})})}}},MO=function(t){return{apply:function(r){return function(e){return OO(t)(t.Monad0().Applicative0())(r)(_(g)(Yi)(e))}},Functor0:function(){return g}}};var C=function(t){return{pure:function(r){return function(e){return yf(t.Monad0().Bind1().Apply0().Functor0())(l(t.Monad0().Applicative0())(void 0))(e(r))}},Apply0:function(){return MO(t)}}};var R=function(t){return{alt:function(r){return function(e){return function(n){return Wt(t.Apply0())(_(t.Apply0().Functor0())(function(a){return function(u){return Q(t.Apply0())(a)(u)}})(r(n)))(e(n))}}},Functor0:function(){return g}}};var E=function(t){return{empty:function(r){return l(t)(l(t)(void 0))},Alt0:function(){return R(t)}}},PO=function(t){return{Applicative0:function(){return C(t)},Plus1:function(){return E(t.Monad0().Applicative0())}}},Vt=function(t){return{fold:FO(t),keepLatest:wO(t),sampleOn:xO(t)(t.Monad0().Applicative0()),fix:$O(t)(t.Monad0()),Alternative0:function(){return PO(t)},Filterable1:function(){return Ou(t.Monad0().Applicative0())}}};function b_(t,r){var e={};for(var n in r)({}).hasOwnProperty.call(r,n)&&(e[n]=r[n]);for(var a in t)({}).hasOwnProperty.call(t,a)&&(e[a]=t[a]);return e}var Tk=function(t){return function(){return function(){return function(r){return function(e){return function(n){return lo(ge(t)(r))(e)(n)}}}}}};var Sk=function(){return function(){return function(t){return function(r){return b_(t,r)}}}},Vc=function(t){return function(){return function(){return function(r){return function(e){return function(n){return lo(ge(t)(r))(e)(n)}}}}}},Qo=function(t){return function(){return function(r){return function(e){return Ga(ge(t)(r))(e)}}}};var Xn={vb:function(t){return function(r){return function(e){return function(n){return l(t.Monad0().Applicative0())(new et({},{}))}}}}},cs=function(t){return t.vb},BO={vbus:function(){var t=function(){return function(e){return function(n){return function(a){return function(u){return Ut(function(i){return P(e.Monad0().Bind1())(cs(n)(e)(d.value)(d.value)(d.value))(function(o){return Xr(Qr)(e.Monad0().Bind1())(i(u(o.value0)(o.value1)))(function(){return l(e.Monad0().Applicative0())(l(e.Monad0().Applicative0())(void 0))})})})}}}}},r=function(){return function(e){return function(n){return t()(e)(n)}}};return r}()},Vv=function(){return function(t){return function(r){return function(e){return function(n){return n()(t)(r)}(BO.vbus)(e)}}}},Qu=function(t){return function(){return function(){return function(){return function(r){return function(e){return function(){return function(){return{vb:function(n){return function(a){return function(u){return function(i){return P(n.Monad0().Bind1())(cs(e)(n)(d.value)(u)(i))(function(o){return P(n.Monad0().Bind1())(cs(r)(n)(d.value)(u)(i))(function(p){return l(n.Monad0().Applicative0())(new et(Vc(t)()()(d.value)(p.value0)(o.value0),Vc(t)()()(d.value)(p.value1)(o.value1)))})})}}}}}}}}}}}}},fe=function(t){return function(){return function(){return function(r){return function(){return function(){return{vb:function(e){return function(n){return function(a){return function(u){return P(e.Monad0().Bind1())(cs(r)(e)(d.value)(a)(u))(function(i){return P(e.Monad0().Bind1())(Lf(e)(e))(function(o){return l(e.Monad0().Applicative0())(new et(Vc(t)()()(d.value)(o.push)(i.value0),Vc(t)()()(d.value)(o.event)(i.value1)))})})}}}}}}}}}}};var Ko=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),y_=function(){function t(){}return t.value=new t,t}();var Bf=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Hf=function(){function t(){}return t.value=new t,t}(),Gv=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),fs=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Uf=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Fi=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),M=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var xk=function(t){return t};var A_={eq:function(t){return function(r){return t instanceof Ko&&r instanceof Ko?t.value0===r.value0:t instanceof y_&&r instanceof y_}}};var U=function(t){return new Uf(t)},ls=function(t){return new Fi(t)},_s=function(t){return new fs(t)};var Fk=t=>r=>r[t];var ti=function(t){return t.reflectType};var $k=function(t){return ti(t)};var k_=$r;var Gc=function(){return function(t){return t}};var Ik=function(t){return[t]};var Rk=function(){return function(){return function(){return function(){return function(){return function(t){var r=$k(t);return function(e){return Fk(r(e))}}}}}}};var Jv=[];var Jc=function(){return function(){return function(t){return function(r){return ic(t)(r)}}}};function Nk(t){return function(){var r={};for(var e in t)hasOwnProperty.call(t,e)&&(r[e]=t[e]);return r}}var ri={};function jv(t){return t()}function Lk(t,r){var e={};for(var n in t)hasOwnProperty.call(t,n)&&(e[n]=r(t[n]));return e}function Bk(t,r){var e={};for(var n in t)hasOwnProperty.call(t,n)&&(e[n]=r(n)(t[n]));return e}function Hk(t){return function(r){return function(e){return function(n){var a=e;function u(o){return function(p){return r(p)(o)(n[o])}}for(var i in n)hasOwnProperty.call(n,i)&&(a=t(a)(u(i)));return a}}}}function g_(t){return function(r){var e=[];for(var n in r)hasOwnProperty.call(r,n)&&e.push(t(n)(r[n]));return e}}var ZO=Object.keys||g_(function(t){return function(){return t}});function Xv(t){return function(r){return function(e){return function(){return e[t]=r,e}}}}var Qv=function(t){return function(r){return function(){return delete r[t],r}}};var Kv=g_(function(t){return function(r){return r}});var QO=Nk;var Wk=function(t){return function(r){return jv(function(){var n=QO(r)();return t(n)(),n})}};var qk=function(t){return function(r){return Bk(r,t)}};var Oi=function(t){return function(r){return Wk(Xv(t)(r))}},ms={map:function(t){return function(r){return Lk(r,t)}}},KO={mapWithIndex:qk,Functor0:function(){return ms}},Yv=function(){return mt};var vs=Hk(Yi),Zk=function(t){return function(r){return vs(function(e){return function(n){return function(a){return bt(t.Semigroup0())(e)(r(n)(a))}}})(Pt(t))}},C_={foldl:function(t){return vs(function(r){return function(e){return t(r)}})},foldr:function(t){return function(r){return function(e){return ae(Bt)(t)(r)(Kv(e))}}},foldMap:function(t){return function(r){return Zk(t)(T(r))}}},zk={foldlWithIndex:function(t){return vs(Mt(t))},foldrWithIndex:function(t){return function(r){return function(e){return ae(Bt)(Of(t))(r)(g_(et.create)(e))}}},foldMapWithIndex:function(t){return Zk(t)},Foldable0:function(){return C_}},YO={traverseWithIndex:function(t){return function(r){return function(e){return vs(function(n){return function(a){return function(u){return Wt(t.Apply0())(_(t.Apply0().Functor0())(Mt(Oi(a)))(n))(r(a)(u))}}})(l(t)(ri))(e)}}},FunctorWithIndex0:function(){return KO},FoldableWithIndex1:function(){return zk},Traversable2:function(){return Wf}},Wf={traverse:function(t){var r=cc(YO)(t);return function(e){return r(T(e))}},sequence:function(t){return Nn(Wf)(t)(rt(nt))},Functor0:function(){return ms},Foldable1:function(){return C_}};var tD=function(t){return Wk(Qv(t))};var Vk=function(){function t(){}return t.value=new t,t}(),rD=function(){function t(){}return t.value=new t,t}(),t$=function(){function t(){}return t.value=new t,t}();var Gk=function(t){return function(r){return function(e){var n=function(a){var u=function(i){return function(o){return new et(o+1|0,new et(i,o))}};return v_(Vt(t))(u)(a)(0)};return new fs(Ce(Vt(t))(Xo(t)(n(e))(function(a){return _(g)(function(u){return I(R(t.Monad0().Applicative0()))(l(C(t))(new Bf(r(u.value0))))(_(g)(T(Hf.value))(l_(Ou(t.Monad0().Applicative0()))(function(){var i=nr(tc)(u.value1+1|0);return function(o){return i(ln(o))}}())(a)))})(a)})))}}};var Yu=function(t){return function(r){return function(e){return function(n){return function(a){var u=function(i){return i(n)(a)};return function(i){if(i instanceof Uf)return Cn(Bt)(E(t))(Yu(t)(r)(e)(n)(a))(i.value0);if(i instanceof Fi)return Ce(Vt(r))(_(g)(Yu(t)(r)(e)(n)(a))(i.value0));if(i instanceof M)return u(e.toElt(i.value0));if(i instanceof fs)return Ut(function(o){return P(r.Monad0().Bind1())(St(r)(fn(ri)))(function(p){return P(r.Monad0().Bind1())($t(i.value0)(function(s){return P(r.Monad0().Bind1())(e.ids(a))(function(f){return P(r.Monad0().Bind1())(St(r)(fn(l(t)(void 0))))(function(m){return P(r.Monad0().Bind1())(e.ids(a))(function(v){return P(r.Monad0().Bind1())(St(r)(fn(l(t)(void 0))))(function(c){return P(r.Monad0().Bind1())(St(r)(fn([])))(function(h){return P(r.Monad0().Bind1())(St(r)(fn(l(t)(void 0))))(function(ut){return P(r.Monad0().Bind1())(_(t.Apply0().Functor0())(Ko.create)(e.ids(a)))(function(dt){return P(r.Monad0().Bind1())(St(r)(fn(Vk.value)))(function(fr){return P(r.Monad0().Bind1())($t(s)(function(Zt){return P(r.Monad0().Bind1())(St(r)(We(fr)))(function(Kr){return Zt instanceof Gv&&Kr instanceof rD?P(r.Monad0().Bind1())(St(r)(We(h)))(ce(t)(Bt)(function(){var re=e.doLogic(Zt.value0)(a);return function(pt){return o(re(pt))}}())):Zt instanceof Hf&&Kr instanceof rD?Xr(Qr)(r.Monad0().Bind1())(cr(t.Apply0().Functor0())(St(r)(gn(t$.value)(fr))))(function(){var re=Q(t.Apply0())(Q(t.Apply0())(Q(t.Apply0())(Q(t.Apply0())(P(r.Monad0().Bind1())(St(r)(We(h)))(ce(t)(Bt)(function(pt){return jn(t)(te)(n.parent)(function(mr){return o(e.disconnectElement(a)({id:pt,parent:mr,scope:dt}))})})))(nu(r.Monad0().Bind1())(St(r)(We(m)))))(nu(r.Monad0().Bind1())(St(r)(We(c)))))(cr(t.Apply0().Functor0())(St(r)(fu(tD(f))(p)))))(cr(t.Apply0().Functor0())(St(r)(fu(tD(v))(p))));return Q(t.Apply0())(cr(t.Apply0().Functor0())(St(r)(gn(re)(ut))))(re)}):Zt instanceof Bf&&Kr instanceof Vk?Xr(Qr)(r.Monad0().Bind1())(cr(t.Apply0().Functor0())(St(r)(gn(rD.value)(fr))))(function(){return P(r.Monad0().Bind1())($t(Yu(t)(r)(e)(function(){var re={};for(var pt in n)({}).hasOwnProperty.call(n,pt)&&(re[pt]=n[pt]);return re.scope=dt,re.raiseId=function(mr){return cr(t.Apply0().Functor0())(St(r)(fu(bt(cn)([mr]))(h)))},re}())(a)(Zt.value0))(o))(function(re){return Xr(Qr)(r.Monad0().Bind1())(cr(t.Apply0().Functor0())(St(r)(fu(Oi(v)(re))(p))))(function(){return cr(t.Apply0().Functor0())(St(r)(gn(re)(c)))})})}):l(t)(void 0)})}))(function(Zt){return Xr(Qr)(r.Monad0().Bind1())(cr(t.Apply0().Functor0())(St(r)(gn(Zt)(m))))(function(){return Xr(Qr)(r.Monad0().Bind1())(cr(t.Apply0().Functor0())(St(r)(fu(Oi(f)(Zt))(p))))(function(){return nu(r.Monad0().Bind1())(St(r)(We(ut)))})})})})})})})})})})})}))(function(s){return l(t)(Xr(Qr)(r.Monad0().Bind1())(P(r.Monad0().Bind1())(St(r)(We(p)))(be(C_)(Q(t.Apply0()))(l(t)(void 0))))(function(){return s}))})})});throw new Error("Failed pattern match at Bolson.Control (line 531, column 17 - line 615, column 20): "+[i.constructor.name])}}}}}},r$=function(){return function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(i){var o=function(p){return function(s){return Ut(function(f){return P(t.Monad0().Bind1())(St(t)(ok(_($r)(T(""))(Gc()(u)))))(function(m){var v=xe(Bt)(E(t.Monad0().Applicative0()))(yo(ki)(function(c){return Xm(xy)(function(h){return function(ut){return ut instanceof M?function(dt){return dt(function(){var fr={};for(var Zt in p)({}).hasOwnProperty.call(p,Zt)&&(fr[Zt]=p[Zt]);return fr.parent=W.value,fr.scope=e(p.scope),fr.raiseId=function(Kr){return St(t)(ik(c)(Kr)(m))},fr}())(s)}(a.toElt(ut.value0)):h(a.wrapElt(ut))}})})(Gc()(u)));return P(t.Monad0().Bind1())($t(v)(f))(function(c){return P(t.Monad0().Bind1())(St(t)(fn(l(t.Monad0().Applicative0())(void 0))))(function(h){return P(t.Monad0().Bind1())(_(t.Monad0().Bind1().Apply0().Functor0())(mt)(St(t)(ck(m))))(function(ut){var dt=_(k_)(function(Zt){return function(Kr){return new M(a.fromEltO1(function(re){return function(pt){return Ut(function(mr){return Xr(Qr)(t.Monad0().Bind1())(re.raiseId(Zt))(function(){return Xr(Qr)(t.Monad0().Bind1())(jn(t.Monad0().Applicative0())(te)(re.parent)(function(Fr){return mr(a.giveNewParent(pt)({id:Zt,parent:Fr,scope:re.scope})(Kr))}))(function(){return l(t.Monad0().Applicative0())(l(t.Monad0().Applicative0())(void 0))})})})}}))}})(ut),fr=Yu(t.Monad0().Applicative0())(t)(n)(p)(s)(i(dt)(mt));return P(t.Monad0().Bind1())($t(fr)(f))(function(Zt){return Xr(Qr)(t.Monad0().Bind1())(cr(t.Monad0().Bind1().Apply0().Functor0())(St(t)(gn(Zt)(h))))(function(){return l(t.Monad0().Applicative0())(Xr(Qr)(t.Monad0().Bind1())(c)(function(){return Xr(Qr)(t.Monad0().Bind1())(zn(t.Monad0().Applicative0())(!r)(jn(t.Monad0().Applicative0())(Bt)(Gc()(ut))(function(Kr){return f(a.deleteFromCache(s)({id:Kr}))})))(function(){return nu(t.Monad0().Bind1())(St(t)(We(h)))})}))})})})})})})})}};return new M(a.fromEltO2(o))}}}}}}}};var eD=function(){return function(t){return function(r){return function(e){return function(n){return function(a){return r$()(t)(!1)(rt(nt))(r)(e)(n)(a)}}}}}};var Jk=function(t){return function(r){return function(e){return function(n){var a=function(u){return function(i){return Ut(function(o){return P(t.Monad0().Bind1())(St(t)(fn(W.value)))(function(p){var s=n(new M(e.fromElt(function(f){return function(m){return Ut(function(v){return Xr(Qr)(t.Monad0().Bind1())(P(t.Monad0().Bind1())(St(t)(We(p)))(function(c){if(c instanceof W)return l(t.Monad0().Applicative0())(void 0);if(c instanceof F)return jn(t.Monad0().Applicative0())(te)(f.parent)(function(h){return zn(t.Monad0().Applicative0())(c.value0!==h)(Q(t.Monad0().Bind1().Apply0())(f.raiseId(c.value0))(v(e.connectToParent(i)({id:c.value0,parent:h}))))});throw new Error("Failed pattern match at Bolson.Control (line 641, column 36 - line 648, column 16): "+[c.constructor.name])}))(function(){return l(t.Monad0().Applicative0())(l(t.Monad0().Applicative0())(void 0))})})}})));return $t(Yu(t.Monad0().Applicative0())(t)(r)(function(){var f={};for(var m in u)({}).hasOwnProperty.call(u,m)&&(f[m]=u[m]);return f.parent=u.parent,f.scope=u.scope,f.raiseId=function(v){return Xr(Qr)(t.Monad0().Bind1())(u.raiseId(v))(function(){return cr(t.Monad0().Bind1().Apply0().Functor0())(St(t)(gn(new F(v))(p)))})},f}())(i)(s))(o)})})}};return new M(e.fromElt(a))}}}};var f$=function(){return function(){return function(){return function(t){return function(r){return function(e){return Nl(e.type)(t)?Ga(e.type)(t)(e.value):r(e)}}}}}};var le=function(){return function(t){return function(r){return function(e){return{type:ge(t)(r),value:e}}}}};var Kk=function(t){return _u("Data.Variant: pattern match failure ["+(t.type+"]"))},Ge=function(){return function(){return function(){return function(t){return f$()()()(t)(Kk)}}}};function Yk(t){var r={};for(var e in t)({}).hasOwnProperty.call(t,e)&&(r[e]=t[e]);return r}function tg(t){return function(r){return function(e){return e[t]=r,e}}}function rg(t){return function(r){return function(e){return e[t]=r(e[t]),e}}}var h_=bi;var eg=function(){return function(){return function(t){return function(r){return function(e){return function(n){return rg(ge(t)(r))(e)(n)}}}}}};var aD=function(){return function(){return function(t){return function(r){return function(e){return function(n){return tg(ge(t)(r))(e)(n)}}}}}};var E_=nt,ds=function(t){return function(r){return t(Yk(r))}},ng=Mt(ds)({});var p$=function(t){return t},s$=function(t){return t.mappingWithIndex};var m$=function(t){return t.mapping};var Qn={mapRecordWithIndexBuilder:function(t){return function(r){return rt(E_)}}},ag=function(t){return t.mapRecordWithIndexBuilder},Wr=function(t){return function(r){return function(e){return function(){return function(){return{mapRecordWithIndexBuilder:function(n){return function(a){return Wu(h_)(eg()()(t)(d.value)(s$(r)(a)(d.value)))(ag(e)(d.value)(a))}}}}}}}};var su=function(){return function(t){return{hmap:function(){var r=ag(t)(d.value);return function(e){return ds(r(p$(e)))}}()}}};var oD=function(t){return t.hmap},qr=function(t){return{mappingWithIndex:function(r){return function(e){return m$(t)(r)}}}};var ug=function(){function t(){}return t.value=new t,t}();var ro=function(t){return{mapping:function(r){return oD(t)(ug.value)}}},_e={mapping:function(t){return _(qn)(_t)}};var v$=function(t){return oD(t)(ug.value)},eo=function(){return function(){return function(t){return function(r){return function(e){return function(n){return new Fi(Vv()(V)(r)(e)(ka(en)(v$(su()(t)))(n)))}}}}}};var iD=function(t){return Zv(V)(ka(en)(_(qn)(_t))(t))};var nn=function(t){return new Fi(iD(t))},og=function(t){return nn(Hc(t))};var b$=function(t){return t.makeText},y$=function(t){return function(r){return function(e){return _(g)(function(n){return t.setText(function(a){return{id:r,text:a}}(n))})(e)}}},A$=function(t){return function(r){return function(e){return _(g)(function(n){return function(a){if(a.value instanceof __)return t.setProp({id:r,key:a.key,value:a.value.value0});if(a.value instanceof p_)return t.setCb({id:r,key:a.key,value:a.value.value0});throw new Error("Failed pattern match at Deku.Control (line 88, column 26 - line 90, column 45): "+[a.value.constructor.name])}(Yp(n))})(e)}}},k$=function(t){return t.makeElement},g$=function(t){return t.attributeParent},mn=function(t){var r=function(e){return function(n){return Ut(function(a){return P(Co)(n.ids)(function(u){return Xr(Qr)(Co)(e.raiseId(u))(function(){return _(ft)(Q(zc)(a(n.deleteFromCache({id:u}))))($t(xe(Bt)(E(D))([l(C(V))(b$(n)({id:u,parent:e.parent,scope:e.scope})),y$(n)(u)(t)]))(a))})})})}};return new M(r)},pe=function(t){return mn(l(C(V))(t))},ig=function(t){return function(r){return function(e){return t(O(E(D)))([Gk(V)(r)(e)])}}};var C$=function(t){return{doLogic:function(r){return function(e){return function(n){return e.sendToPos({id:n,pos:r})}}},ids:function(){var r=Te(t);return function(e){return function(n){return n.ids}(r(e))}}(),disconnectElement:function(r){return function(e){return r.disconnectElement({id:e.id,scope:e.scope,parent:e.parent,scopeEq:nr(A_)})}},toElt:function(r){return r}}};var cg=Yu(D)(V)(C$()),fg=function(t){return function(r){return function(e){return Ut(function(n){return P(Co)(e.ids)(function(a){return $t(I(R(D))(l(C(V))(e.makeRoot({id:a,root:t})))(cg({parent:new F(a),scope:new Ko("rootScope"),raiseId:function(u){return l(D)(void 0)},pos:W.value})(e)(r)))(n)})})}}};var X=function(t){return function(r){return function(e){var n=function(a){return function(u){return Ut(function(i){return P(Co)(u.ids)(function(o){return Xr(Qr)(Co)(a.raiseId(o))(function(){return _(ft)(function(p){return Q(zc)(i(u.deleteFromCache({id:o})))(p)})($t(I(R(D))(xe(Bt)(E(D))(bt(cn)([l(C(V))(k$(u)({id:o,parent:a.parent,scope:a.scope,tag:t})),A$(u)(o)(r)])(Jt([])(function(p){return[l(C(V))(g$(u)({id:o,parent:p,pos:a.pos}))]})(a.parent))))(cg({parent:new F(o),scope:a.scope,raiseId:function(p){return l(D)(void 0)},pos:W.value})(u)(e)))(i))})})})}};return n}}};var De=function(){function t(){}return t.value=new t,t}();var ye={attr:function(t){return function(r){return b({key:"click",value:st(r)})}}};var Xt=function(){function t(){}return t.value=new t,t}();var bs={attr:function(t){return function(r){return b({key:"style",value:q(r)})}}};var lg={attr:function(t){return function(r){return b({key:"style",value:q(r)})}}};var Dt={attr:function(t){return function(r){return b({key:"style",value:q(r)})}}};var _g={attr:function(t){return function(r){return b({key:"style",value:q(r)})}}},qf={attr:function(t){return function(r){return b({key:"style",value:q(r)})}}};var cD={attr:function(t){return function(r){return b({key:"style",value:q(r)})}}};var pg={attr:function(t){return function(r){return b({key:"style",value:q(r)})}}};var fD=function(t){return function(r){return new M(X("a")(t)(U(r)))}};var hr=function(t){return function(r){return new M(X("div")(t)(U(r)))}},zr=hr(O(E(D)));var zf=function(t){return function(r){return new M(X("span")(t)(U(r)))}},lD=zf(O(E(D)));var E$=function(t){return function(r){return og(function(e){return ls(Xo(V)(t(e.value1))(function(n){return r(new et(e.value0,n))}))})}},mg=function(t){return E$(function(r){return I(R(D))(l(C(V))(t))(r)})};var vg=function(t){return function(r){return t(r)}};var Dg=(t,r,e,n)=>{t(a=>n.units[a].main.appendChild(n.units[r].main))(e)};var dg=t=>r=>()=>{r.units[t.id].main.parentNode||(typeof t.pos.value0=="number"&&r.units[t.parent].main.children[t.pos.value0]?r.units[t.parent].main.insertBefore(r.units[t.id].main,r.units[t.parent].main.children[t.pos.value0]):t.parent.indexOf("@!%")!==-1?r.units[t.parent].main.parentNode.replaceChild(r.units[t.id].main,r.units[t.parent].main):r.units[t.parent].main.appendChild(r.units[t.id].main))},bg=t=>r=>e=>()=>{var n,a=r.id;e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(a),e.hydrating&&t&&r.parent.value0&&(n=document.body.querySelectorAll("[data-deku-ssr-"+a+"]").item(0))?e.units[a]={listeners:{},parent:r.parent,scope:r.scope,main:n}:e.units[a]={listeners:{},parent:r.parent,scope:r.scope,main:document.createElement(r.tag)}},yg=t=>r=>e=>n=>()=>{var a=e.id,u;if(n.scopes[e.scope]||(n.scopes[e.scope]=[]),n.scopes[e.scope].push(a),n.hydrating&&t&&e.parent.value0&&(u=document.body.querySelectorAll("[data-deku-ssr-"+e.parent.value0+"]").item(0))){var i=0;if(u.childNodes.length===1)u.prepend(document.createTextNode(""));else for(var i=0;i<u.childNodes.length;i++)if(u.childNodes[i].nodeType===8&&u.childNodes[i].nodeValue===a){i=i-1;break}n.units[a]={main:u.childNodes[i],parent:e.parent,scope:e.scope}}else n.units[a]={main:document.createTextNode(""),parent:e.parent,scope:e.scope},Dg(r,a,e.parent,n)};function _D(){return{units:{},scopes:{}}}var Ag=t=>r=>e=>()=>{var n=r.id,a=r.value;e.hydrating&&t&&!e.units[n]&&(dom=document.body.querySelectorAll("[data-deku-ssr-"+n+"]").item(0))&&(e.units[n]={listeners:{},parent:r.parent,scope:r.scope,main:dom},e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(n)),e.units[n].main.tagName==="INPUT"&&r.key==="value"?e.units[n].main.value=a:e.units[n].main.tagName==="INPUT"&&r.key==="checked"?e.units[n].main.checked=a==="true":e.units[n].main.setAttribute(r.key,a)},kg=t=>r=>e=>()=>{var n=r.id,a=r.value;if(e.hydrating&&t&&!e.units[n]&&(dom=document.body.querySelectorAll("[data-deku-ssr-"+n+"]").item(0))&&(e.units[n]={listeners:{},parent:r.parent,scope:r.scope,main:dom},e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(n)),r.key==="@self@")a(e.units[n].main)();else{e.units[n].listeners[r.key]&&e.units[n].main.removeEventListener(r.key,e.units[n].listeners[r.key]);var u=i=>a(i)();e.units[n].main.addEventListener(r.key,u),e.units[n].listeners[r.key]=u}},gg=t=>r=>()=>{var e=t.id;r.units[e].main.nodeValue=t.text},Cg=t=>r=>e=>n=>()=>{var a,u,i=e.id,o=e.html,p=e.verb,s=e.cache,f=e.parent,m=e.scope,v=e.pxScope;if(n.hydrating&&t&&e.parent.value0&&(a=document.body.querySelectorAll("[data-deku-ssr-"+i+"]").item(0)))n.units[i]={listeners:{},scope:m,parent:f,main:a};else{let h=Object.entries(s);for(var c=0;c<h.length;c++){let ut=h[c][0];h[c][1]===!0?o=o.replace(p+ut+p,'data-deku-attr-internal="'+ut+'"'):o=o.replace(p+ut+p,'<span style="display:contents;" data-deku-elt-internal="'+ut+'"></span>')}u=document.createElement("div"),u.innerHTML=o.trim(),n.units[i]={listeners:{},scope:m,parent:f,main:u.firstChild}}n.scopes[m]||(n.scopes[m]=[]),n.scopes[m].push(i),u||(u=a),u.querySelectorAll("[data-deku-attr-internal]").forEach(function(h){var ut=h.getAttribute("data-deku-attr-internal");let dt=ut+"@!%"+v;n.units[dt]={listeners:{},main:h,scope:m},n.scopes[m].push(dt)}),u.querySelectorAll("[data-deku-elt-internal]").forEach(function(h){var ut=h.getAttribute("data-deku-elt-internal");let dt=ut+"@!%"+v;n.units[ut+"@!%"+v]={listeners:{},main:h,scope:m},n.scopes[m].push(dt)}),a||Dg(r,i,f,n)},hg=t=>r=>()=>{var e=t.id;r.units[e]={main:t.root}},Eg=t=>r=>()=>{var e=t.id,n=t.parent;r.units[e].containingScope=t.scope,r.units[n].main.prepend(r.units[e].main)},Tg=t=>r=>()=>{var e=t.id;r.units[e].noop||r.units[e].containingScope&&!t.scopeEq(r.units[e].containingScope)(t.scope)||r.units[e].main.remove()},Sg=t=>r=>()=>{delete r.units[t.id]},xg=t=>r=>()=>{var e=t.id,n=t.pos,a=r.units[e].main.parentNode;a.insertBefore(r.units[e].main,a.children.length<=n?a.children[a.children.length-1]:n<0?a.children[0]:a.children[n])};var Fg=function(t){return function(r){return function(e){return(e|0)===e?t(e):r}}},jr=function(t){return t};var pD=function(t){return function(r){return Math.pow(t,r)|0}};var ys=isFinite;var T_=Math.floor;var Dc=function(t){return function(r){return Math.pow(t,r)}},S_=function(t){return function(r){return t%r}},As=Math.round;var ks=Math.sin;var dc=3.141592653589793;var Vf=function(){return Fg(F.create)(W.value)}(),$g=function(t){if(!ys(t))return 0;if(t>=jr(oa(Ic)))return oa(Ic);if(t<=jr(ia(Ic)))return ia(Ic);if(oe)return Aa(0)(Vf(t));throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): "+[t.constructor.name])},wg=function(t){return $g(As(t))};var x_=function(t){return $g(T_(t))};var no=Math.random;var F_=function(t){return function(r){return function(){var n=no(),a=(jr(r)-jr(t)+1)*n+jr(t);return x_(a)}}};var Mg=function(t){return t};var M$=1,gs=2147483647,P$=function(){return gs-1|0}(),Jf=function(t){var r=function(e){return function(n){return function(a){var u=n-e|0,i=ou(mo)(a)(u),o=i<e;return o?i+n|0:i}}};return r(M$)(P$)(t)};var I$=0,R$=48271,Pg=function(t){return function(r){return fa()(Vf(S_(jr(R$)*jr(r)+jr(t))(jr(gs))))}},Ig=Pg(I$);var Z$=function(){function t(i){this.fn=i}var r={},e=function(i,o){this.head=i,this.tail=o};function n(i){return new e(i,r)}function a(i){return function(o){return new e(i,o)}}function u(i){for(var o=[],p=i;p!==r;)o.push(p.head),p=p.tail;return o}return function(i){return function(o){return function(p){var s=function(m,v){return i(o(a)(p(m)))(v)},f=function(m,v,c){if(v===0)return m;var h=c[v-1];return new t(function(){var ut=f(s(h,m),v-1,c);return ut})};return function(m){for(var v=o(n)(p(m[m.length-1])),c=f(v,m.length-1,m);c instanceof t;)c=c.fn();return o(u)(c)}}}}}();var Hg=function(t){return t};var Ug=cn;var Wg=Bt;var zg=Hg,$_=function(t){return t};var w_=function(t){return zg(BA(t))};var jf=function(t){if(Oa(t)>0)return new F(zg(t));if(oe)return W.value;throw new Error("Failed pattern match at Data.Array.NonEmpty (line 157, column 1 - line 157, column 58): "+[t.constructor.name])};var Vg=function(t){return function(r){return t($_(r))}};var Gg=Vg(Oa);var Jg=function(){return Vg(Ev())};var $i=function(t){return t.state};function ai(t){return new Error(t)}function Qc(t){return function(){throw t}}function Ts(t){return function(r){return function(){try{return r()}catch(e){return e instanceof Error||Object.prototype.toString.call(e)==="[object Error]"?t(e)():t(new Error(e.toString()))()}}}}var Eo=function(t){return t.throwError};var Cw={throwError:Qc,Monad0:function(){return qo}};var gD={catchError:Mt(Ts),MonadThrow0:function(){return Cw}};var wi=function(t){return t.catchError};var P_=function(t){return function(r){return wi(t)(_(t.MonadThrow0().Monad0().Bind1().Apply0().Functor0())(Yt.create)(r))(function(){var e=l(t.MonadThrow0().Monad0().Applicative0());return function(n){return e(Kt.create(n))}}())}};var se={liftEffect:rt(nt),Monad0:function(){return qo}},Ae=function(t){return t.liftEffect};var ws=function(t){return{map:function(r){return function(e){return function(n){return _(t)(function(a){return new et(r(a.value0),a.value1)})(e(n))}}}}};var CD=function(t){return{Applicative0:function(){return Is(t)},Bind1:function(){return Ms(t)}}},Ms=function(t){return{bind:function(r){return function(e){return function(n){return P(t.Bind1())(r(n))(function(a){var u=e(a.value0);return u(a.value1)})}}},Apply0:function(){return Ps(t)}}},Ps=function(t){return{apply:_o(CD(t)),Functor0:function(){return ws(t.Bind1().Apply0().Functor0())}}},Is=function(t){return{pure:function(r){return function(e){return l(t.Applicative0())(new et(r,e))}},Apply0:function(){return Ps(t)}}};var hD=function(t){return{state:function(r){var e=l(t.Applicative0());return function(n){return e(r(n))}},Monad0:function(){return CD(t)}}};var eC=function(t){return function(r){var e=t(r);return e.value0}};var Ow=function(t){return t};var uC=function(){var t=function(r){return new et(Mg(r.newSeed),function(){var e={};for(var n in r)({}).hasOwnProperty.call(r,n)&&(e[n]=r[n]);return e.newSeed=Ig(r.newSeed),e}())};return $i(hD(zu))(t)}();var To=ws(so),oC=_(To)(function(t){return jr(t)/jr(gs)})(uC);var Kf=function(t){return eC(Ow(t))};var Yc=Ms(zu);var tf=Ps(zu),aC=function(t){return function(r){var e=jr(r),n=jr(t),a=function(o){return n+S_(o)(e-n+1)},u=_(To)(jr)(uC),i=Wt(tf)(_(To)(Br($a))(u))(_(To)(Rn($a)(2))(u));return _(To)(function(o){return x_(a(o))})(i)}},TD=function(t){return function(r){var e=t<=r;return e?aC(t)(r):aC(r)(t)}};var B_=Is(zu);var SD=function(t){return P(Yc)(TD(0)(Gg(t)-1|0))(function(r){return l(B_)(Jg()(t)(r))})};var U_=function(t){return t.arbitrary};var iC={arbitrary:oC};var cC=function(){return{arbitrary:TD(-1e6)(1e6)}}();var fC=function(t){return{ids:us(Tf)(function(){var e=Ee(t)(),n=jt(uu)(Kf(U_(cC))({newSeed:Jf(e),size:5}));return cr(S)(Sf(Br(qu)(1))(t))(),n}),makeElement:bg(!1),attributeParent:dg,makeRoot:hg,makeText:yg(!1)(Jt(void 0)),makePursx:Cg(!1)(Jt(void 0)),setProp:Ag(!1),setCb:kg(!1),setText:gg,sendToPos:xg,deleteFromCache:Sg,giveNewParent:Eg,disconnectElement:Tg}};var Mi=function(){return window};function _C(t,r,e,n){if(typeof window<"u"){var a=window[e];if(a!=null&&n instanceof a)return r(n)}for(var u=n;u!=null;){var i=Object.getPrototypeOf(u),o=i.constructor.name;if(o===e)return r(n);if(o==="Object")return t;u=i}return t}var Rt=function(t){return function(r){return _C(W.value,F.create,t,r)}};function pC(t,r,e){return t==null?r:e(t)}var an=function(t){return pC(t,W.value,F.create)};var FD=Rt("HTMLCanvasElement");function DC(t){return function(){return t.body}}var dC=function(){var t=_(S)(an);return function(r){return t(DC(r))}}();var bC=mt;function rf(t){return function(){return t.valueAsNumber}}var Yf=Rt("HTMLInputElement");function MD(t){return function(){return t.document}}function Ls(t){return function(r){return function(){return r.requestAnimationFrame(t)}}}var PD=mt;var HM=function(t){return function(r){return function(){var n=_D(),a=Gr(S)(ie(0))(function(){var u=fg(t)(r);return function(i){return u(fC(i))}}())();return $t(pu(a))(function(u){return u(n)})()}}};var UM=function(t){return function(){var e=P(Gn)(P(Gn)(Mi)(MD))(dC)();return Jt(Pt(Le(Le(Ne))))(function(n){return HM(n)(t)})(_(Re)(bC)(e))()}},hC=function(t){return cr(S)(UM(t))};var qM=function(t){return t};var Y={pursxToElement:function(t){return function(r){return function(e){return{cache:ri,element:function(n){return function(a){return O(E(D))}}}}}}},ID=function(t){return t.pursxToElement},vn=function(){return function(t){return function(r){return function(e){return{pursxToElement:function(n){return function(a){return function(u){var i=ID(t)(n)(d.value)(u);return{cache:Oi(ti(r)(d.value))(!0)(i.cache),element:function(o){return function(p){return I(R(D))(_(g)(ka(en)(Yp)(function(s){if(s.value instanceof __)return p.setProp({id:ti(r)(d.value)+("@!%"+n),key:s.key,value:s.value.value0});if(s.value instanceof p_)return p.setCb({id:ti(r)(d.value)+("@!%"+n),key:s.key,value:s.value.value0});throw new Error("Failed pattern match at Deku.Pursx (line 4191, column 38 - line 4201, column 24): "+[s.value.constructor.name])}))(Qo(e)()(d.value)(u)))(i.element(o)(p))}}}}}}}}}}};var B=qM,Dr=function(t){return function(r){return function(){return function(){return function(e){return function(n){return function(a){return function(u){var i=function(o){return function(p){return Ut(function(s){return P(Co)(p.ids)(function(f){return P(Co)(p.ids)(function(m){return Xr(Qr)(Co)(o.raiseId(f))(function(){var v=ID(e)(m)(d.value)(u);return _(ft)(Q(zc)(s(p.deleteFromCache({id:f}))))($t(I(R(D))(l(C(V))(p.makePursx({id:f,parent:o.parent,cache:v.cache,pxScope:m,scope:o.scope,html:ti(t)(a),verb:ti(r)(n)})))(v.element(o)(p)))(s))})})})})}};return new M(i)}}}}}}}},It=function(t){return function(){return function(){return function(r){return Dr(t)({reflectType:function(){return"~"}})()()(r)(d.value)}}}};var ZM=Yu(D)(V)({doLogic:function(t){return function(r){return function(e){return r.sendToPos({id:e,pos:t})}}},ids:function(){var t=Te();return function(r){return function(e){return e.ids}(t(r))}}(),disconnectElement:function(t){return function(r){return t.disconnectElement({id:r.id,scope:r.scope,parent:r.parent,scopeEq:nr(A_)})}},toElt:function(t){return t}}),H=function(){return function(t){return function(r){return function(e){return{pursxToElement:function(n){return function(a){return function(u){var i=Qo(e)()(d.value)(u),o=ID(t)(n)(d.value)(u);return{cache:Oi(ti(r)(d.value))(!1)(o.cache),element:function(p){return function(s){return I(R(D))(ZM({parent:new F(ti(r)(d.value)+("@!%"+n)),scope:p.scope,raiseId:function(f){return l(D)(void 0)},pos:p.pos})(s)(i))(o.element(p)(s))}}}}}}}}}}};var gt=function(){return function(){return{defaults:Mt(Sk()())}}},zM=function(t){return t.defaults},Ct={convertRecordOptions:function(t){return function(r){return function(e){return rt(E_)}}}},EC=function(t){return t.convertRecordOptions},ga=function(t){return t.convertOptionsWithDefaults},ht=function(){return function(t){return{convertOptions:function(r){return function(e){return ng(EC(t)(r)(d.value)(e))}}}}},VM=function(t){return t.convertOptions},Et=function(t){return function(r){return{convertOptionsWithDefaults:function(e){return function(n){var a=zM(r)(n),u=VM(t)(e);return function(i){return a(u(i))}}}}}},GM=function(t){return t.convertOption},J=function(t){return function(r){return function(){return function(){return function(){return function(e){return{convertRecordOptions:function(n){return function(a){return function(u){return Wu(h_)(aD()()(e)(d.value)(GM(r)(n)(d.value)(Qo(e)()(d.value)(u))))(EC(t)(n)(d.value)(u))}}}}}}}}}};var RD=function(){var t=Zp(Ov);return function(r){return zp(t(r))}}();var Hut=typeof Array.from=="function",Uut=typeof Symbol<"u"&&Symbol!=null&&typeof Symbol.iterator<"u"&&typeof String.prototype[Symbol.iterator]=="function",Wut=typeof String.prototype.fromCodePoint=="function",qut=typeof String.prototype.codePointAt=="function";var Pi={proof:function(t){return t},Coercible0:function(){}},LD=function(t){return t.proof};var Pu=void 0;var Vs=function(t){return t.toInt},FC=function(t){return function(r){return Vs(t)(Pu)}};var mu={toInt:function(t){return 8}},OC={Nat0:function(){return mu}},ui={toInt:function(t){return 7}},$C={Nat0:function(){return ui}},oi={toInt:function(t){return 6}},wC={Nat0:function(){return oi}},Ua={toInt:function(t){return 5}},Gs={Nat0:function(){return Ua}},Kn={toInt:function(t){return 4}},ma={Nat0:function(){return Kn}},Yn={toInt:function(t){return 3}},Iu={Nat0:function(){return Yn}},ta={toInt:function(t){return 2}},Ru={Nat0:function(){return ta}},ra={toInt:function(t){return 1}},Nu={Nat0:function(){return ra}},Me={toInt:function(t){return 0}};var wr=function(t){return function(){return function(r){return function(){return function(e){return{Nat0:r.Nat1,Pos1:function(){return t}}}}}}};var So={Nat0:function(){return ui},Nat1:function(){return mu}};var xo={Nat0:function(){return oi},Nat1:function(){return mu}};var Fo={Nat0:function(){return Ua},Nat1:function(){return mu}};var Oo={Nat0:function(){return Kn},Nat1:function(){return mu}};var Ca={Nat0:function(){return Kn},Nat1:function(){return Ua}};var $o={Nat0:function(){return Yn},Nat1:function(){return mu}};var ha={Nat0:function(){return Yn},Nat1:function(){return Ua}};var wo={Nat0:function(){return ta},Nat1:function(){return mu}};var Ea={Nat0:function(){return ta},Nat1:function(){return Ua}};var Mo={Nat0:function(){return ra},Nat1:function(){return mu}};var Ta={Nat0:function(){return ra},Nat1:function(){return Ua}};var Po={Nat0:function(){return Me},Nat1:function(){return mu}};var Sa={Nat0:function(){return Me},Nat1:function(){return Ua}};var MC={Nat0:function(){return Me},Nat1:function(){return mu}};var BD={Nat0:function(){return Me},Nat1:function(){return ui}};var HD={Nat0:function(){return Me},Nat1:function(){return oi}};var q_={Nat0:function(){return Me},Nat1:function(){return Ua}};var Ya={Nat0:function(){return Me},Nat1:function(){return Kn}};var Sn={Nat0:function(){return Me},Nat1:function(){return Yn}};var xn={Nat0:function(){return Me},Nat1:function(){return ta}};var Fn={Nat0:function(){return Me},Nat1:function(){return ra}},Lu={Nat0:function(){return Me},Nat1:function(){return Me}};var PC=bo;var Js=function(t){return t};var Z_=function(t){return function(){return function(r){return function(e){return r[Vs(t)(e)]}}}};var js=function(t){return function(r){var e=FC(t)(d.value),n=function(){return e===0?[]:An(0)(e-1|0)}();return _($r)(r)(n)}};var ao=[];var Mr=function(t){return function(r){return function(e){return ic(r)(e)}}};var On={first:function(t){return function(r){return new et(t(r.value0),r.value1)}},second:_(zo),Profunctor0:function(){return en}},ea=function(t){return t.second},Xs=function(t){return t.first};var hP=function(t){return function(r){return function(e){return function(n){return go(e)(t)(r)(n)}}}};var LC=function(){return function(){return function(t){return hP(Jn())(Jn())(t)}}};var BC=function(){return function(){return function(t){return LC()()(t)}}};var SP=function(t){return function(r){return function(e){return go(r.Profunctor0())(t)(function(n){return n.value1(n.value0)})(Xs(r)(e))}}},HC=function(t){return function(r){return function(e){return SP(function(n){return new et(t(n),function(a){return r(n)(a)})})(e)}}};var UC=function(t){return function(){return function(){return function(r){return function(e){return HC(Qo(t)()(r))(Mt(Tk(t)()()(r)))(e)}}}}};var WC=function(t){return t};var MP=JSON.parse;var PP=JSON.stringify;var Qs=function(t){return t};var Ks=function(t){return t};var Ys=function(t){return function(r){return t(r)}},z_=function(t){return{map:function(r){return Ys(_(t)(_(Nc)(r)))}}};var qD=function(t){return{Applicative0:function(){return V_(t)},Bind1:function(){return ZD(t)}}},ZD=function(t){return{bind:function(r){return function(e){return P(t.Bind1())(r)(ja(function(){var n=l(t.Applicative0());return function(a){return n(Kt.create(a))}}())(function(n){var a=e(n);return a}))}},Apply0:function(){return ZC(t)}}},ZC=function(t){return{apply:_o(qD(t)),Functor0:function(){return z_(t.Bind1().Apply0().Functor0())}}},V_=function(t){return{pure:function(){var r=l(t.Applicative0());return function(e){return Qs(r(Yt.create(e)))}}(),Apply0:function(){return ZC(t)}}};var zC=function(t){return{throwError:function(){var r=l(t.Applicative0());return function(e){return Qs(r(Kt.create(e)))}}(),Monad0:function(){return qD(t)}}};var zD=function(t){return function(r){return{alt:function(e){return function(n){return P(r.Bind1())(e)(function(a){if(a instanceof Yt)return l(r.Applicative0())(new Yt(a.value0));if(a instanceof Kt)return P(r.Bind1())(n)(function(u){if(u instanceof Yt)return l(r.Applicative0())(new Yt(u.value0));if(u instanceof Kt)return l(r.Applicative0())(new Kt(bt(t)(a.value0)(u.value0)));throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): "+[u.constructor.name])});throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): "+[a.constructor.name])})}},Functor0:function(){return z_(r.Bind1().Apply0().Functor0())}}}};var VD=function(){var t=Te();return function(r){return t(Ks(r))}}();function G_(t){return Object.prototype.toString.call(t).slice(8,-1)}var GC=Array.isArray||function(t){return Object.prototype.toString.call(t)==="[object Array]"};var jD=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}();var JC=mt;var XD=function(t){var r=Eo(zC(t));return function(e){return r(RD(e))}};var QD=function(t){return function(r){return function(e){if(G_(e)===r)return l(V_(t))(JC(e));if(oe)return XD(t)(new jD(r,G_(e)));throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): "+[r.constructor.name,e.constructor.name])}}};var KD=function(t){return QD(t)("String")};var em=function(){function t(){}return t.value=new t,t}(),nm=function(){function t(){}return t.value=new t,t}(),XC=function(){function t(){}return t.value=new t,t}(),QC=function(){function t(){}return t.value=new t,t}(),td=function(){function t(){}return t.value=new t,t}(),KC=function(){function t(){}return t.value=new t,t}(),YC=function(){function t(){}return t.value=new t,t}();var th=function(t){return t},rh=function(t){return t};var eh=function(t){return t};var nh=function(t){return t};var ah=function(t){return t};var uh=function(t){return t},oh=function(t){return t},ih=function(t){return t},ch=function(t){return t},fh=function(t){return t};var rd=function(){function t(){}return t.value=new t,t}(),lh=function(){function t(){}return t.value=new t,t}(),_h=function(){function t(){}return t.value=new t,t}(),ed=function(){function t(){}return t.value=new t,t}(),ph=function(){function t(){}return t.value=new t,t}();var am=function(t){return t};var nl=function(t){return t};var KP=function(t){return t},J_=function(t){return t};var af={toAudioOnOff:rt(nt)};var uf=function(t){return t.toAudioParameter},sh=function(t){return t.toAudioOnOff},mh=function(){return Bf.create}(),vh=function(){return Hf.value}();var um=function(){return WC(function(){var t=BC()()(en),r=UC({reflectSymbol:function(){return"o"}})()()(d.value)(On);return function(e){return t(r(e))}}())},Dh=mt;var YP=function(){var t=le()({reflectSymbol:function(){return"unit"}})(d.value);return function(r){return J_(t(r))}}();var tI=function(t){return function(r){return{toAudioParameter:function(e){return YP(e)}}}},dh=function(t){return function(r){return{toAudioParameter:function(){var e=uf(tI(t)(r));return function(n){return e(KP(function(a){return{u:a}}(n)))}}()}}},bh=function(){return le()({reflectSymbol:function(){return"2x"}})(d.value)(void 0)}(),yh=function(){var t=le()({reflectSymbol:function(){return"sudden"}})(d.value);return function(r){return J_(t(r))}}();var Ah={toAudioParameter:yh},om={toAudioParameter:function(t){return yh({n:t})}},nd=function(){return le()({reflectSymbol:function(){return"step"}})(d.value)(void 0)}();var ad=function(){return le()({reflectSymbol:function(){return"on"}})(d.value)(void 0)}(),j_={x:ad,o:0},vt=function(){return l(C(G))(_n()(le()({reflectSymbol:function(){return"onOff"}})(d.value)(j_)))};var kh=function(){return le()({reflectSymbol:function(){return"off"}})(d.value)(void 0)}();var rI=function(){var t=le()({reflectSymbol:function(){return"numeric"}})(d.value);return function(r){return J_(t(r))}}();var qe={toAudioParameter:rI};var ii=function(){return le()({reflectSymbol:function(){return"linear"}})(d.value)(void 0)}();var gh=function(){return le()({reflectSymbol:function(){return"exponential"}})(d.value)(void 0)}(),eI=function(){var t=le()({reflectSymbol:function(){return"envelope"}})(d.value);return function(r){return J_(t(r))}}();var Hn={toAudioParameter:eI},nI=function(){var t=le()({reflectSymbol:function(){return"cancel"}})(d.value);return function(r){return J_(t(r))}}();var Ch={toAudioParameter:nI};var aI=function(){function t(){}return t.value=new t,t}(),uI=function(){function t(){}return t.value=new t,t}(),oI=function(){function t(){}return t.value=new t,t}(),iI=function(){function t(){}return t.value=new t,t}(),cI=function(){function t(){}return t.value=new t,t}(),fI=function(){function t(){}return t.value=new t,t}(),lI=function(){function t(){}return t.value=new t,t}(),_I=function(){function t(){}return t.value=new t,t}(),pI=function(){function t(){}return t.value=new t,t}(),sI=function(){function t(){}return t.value=new t,t}(),mI=function(){function t(){}return t.value=new t,t}(),vI=function(){function t(){}return t.value=new t,t}(),DI=function(){function t(){}return t.value=new t,t}(),dI=function(){function t(){}return t.value=new t,t}(),Ii=function(t){return{toPeriodicOscSpec:function(r){return le()({reflectSymbol:function(){return"realImg"}})(d.value)({real:Js(r.value0),img:Js(r.value1)})}}};var im={toInitializeTriangleOsc:function(t){return fh(function(r){return{frequency:r}}(t))}};var hh={toInitializeStereoPanner:function(t){return ch(function(r){return{pan:r}}(t))}};var al={toInitializeSquareOsc:function(t){return ih(function(r){return{frequency:r}}(t))}};var kc={toInitializeSinOsc:function(t){return oh(function(r){return{frequency:r}}(t))}};var Eh={toInitializeSawtoothOsc:function(t){return uh(function(r){return{frequency:r}}(t))}};var ud={toInitializeRecorder:function(t){return th(function(r){return{cb:r}}(t))}};var X_={toInitializeMicrophone:function(t){return rh(function(r){return{microphone:r}}(t))}};var Th=function(t){return function(r){return{toInitializeIIRFilter:function(e){return function(n){return function(a){return{feedforward:LD(Pi)(Jn()(e.value0)),feedback:LD(Pi)(Jn()(e.value1))}}}}}}};var lt={toInitializeGain:function(t){return ah(function(r){return{gain:r}}(t))}};var Sh={toInitializeConvolver:function(t){return eh(function(r){return{buffer:r}}(t))}},cm={toInitializeConstant:function(t){return nh(function(r){return{offset:r}}(t))}};var bI={convertOption:function(t){return function(r){return rt(nt)}}},Q_={convertOption:function(t){return function(r){return rt(nt)}}},xh={convertOption:function(t){return function(r){return rt(nt)}}},Fh={convertOption:function(t){return function(r){return F.create}}},Oh={convertOption:function(t){return function(r){return rt(nt)}}},Ri={convertOption:function(t){return function(r){return rt(nt)}}},ul={convertOption:function(t){return function(r){return rt(nt)}}},ol={convertOption:function(t){return function(r){return rt(nt)}}},il={convertOption:function(t){return function(r){return rt(nt)}}},cl={convertOption:function(t){return function(r){return rt(nt)}}},fl={convertOption:function(t){return function(r){return rt(nt)}}},$h={convertOption:function(t){return function(r){return rt(nt)}}},wh={convertOption:function(t){return function(r){return rt(nt)}}},Mh={convertOption:function(t){return function(r){return rt(nt)}}},od={convertOption:function(t){return function(r){return rt(nt)}}},of={convertOption:function(t){return function(r){return rt(nt)}}},K_={convertOption:function(t){return function(r){return rt(nt)}}},Y_={convertOption:function(t){return function(r){return rt(nt)}}};var ll={convertOption:function(t){return function(r){return rt(nt)}}},Ph={convertOption:function(t){return function(r){return rt(nt)}}},Ih={convertOption:function(t){return function(r){return rt(nt)}}},Rh={convertOption:function(t){return function(r){return rt(nt)}}},id={convertOption:function(t){return function(r){return rt(nt)}}};var Nh={convertOption:function(t){return function(r){return rt(nt)}}},cd={convertOption:function(t){return function(r){return rt(nt)}}},$n={convertOption:function(t){return function(r){return rt(nt)}}},Dn={convertOption:function(t){return function(r){return rt(nt)}}},fd={convertOption:function(t){return function(r){return rt(nt)}}},fm={convertOption:function(t){return function(r){return rt(nt)}}},yI=function(t){return t.toPeriodicOscSpec},Ni=function(t){return{convertOption:function(r){return function(e){return yI(t)}}}},ld=function(t){return t.toInitializeWaveShaper},Lh=function(t){return t.toInitializeTriangleOsc},Bh=function(t){return t.toInitializeStereoPanner},Hh=function(t){return t.toInitializeSquareOsc},Uh=function(t){return t.toInitializeSinOsc},Wh=function(t){return t.toInitializeSawtoothOsc},qh=function(t){return t.toInitializeRecorder},_d=function(t){return t.toInitializePlayBuf},Zh=function(t){return t.toInitializePeriodicOsc},zh=function(t){return t.toInitializePeaking},Vh=function(t){return t.toInitializeNotch},Gh=function(t){return t.toInitializeMicrophone},Jh=function(t){return t.toInitializeLowshelf},pd=function(t){return t.toInitializeLowpass},sd=function(t){return t.toInitializeLoopBuf},jh=function(t){return t.toInitializeIIRFilter},Xh=function(t){return t.toInitializeHighshelf},md=function(t){return t.toInitializeHighpass},Qh=function(t){return t.toInitializeGain},Kh=function(t){return t.toInitializeDynamicsCompressor},vd=function(t){return t.toInitializeDelay},Yh=function(t){return t.toInitializeConvolver},tE=function(t){return t.toInitializeConstant},Dd=function(t){return t.toInitializeBandpass},dd=function(t){return t.toInitializeAllpass};var AI={oversample:bh},kI=function(t){return{toInitializeWaveShaper:function(r){return ga(t)(aI.value)(AI)(r)}}},rE={toInitializeWaveShaper:function(){var t=ld(kI(Et(ht()(J(Ct)(bI)()()()({reflectSymbol:function(){return"curve"}})))(gt()())));return function(r){return t(function(e){return{curve:e}}(r))}}()},gI=function(){return{bufferOffset:0,playbackRate:1,duration:W.value}}(),tp=function(t){return{toInitializePlayBuf:function(r){return ga(t)(uI.value)(gI)(r)}}},tu={toInitializePlayBuf:function(){var t=_d(tp(Et(ht()(J(Ct)(Q_)()()()({reflectSymbol:function(){return"buffer"}})))(gt()())));return function(r){return t(function(e){return{buffer:e}}(r))}}()},CI={},Li=function(t){return{toInitializePeriodicOsc:function(r){return ga(t)(oI.value)(CI)(r)}}},hI={q:1,gain:0},_l=function(t){return{toInitializePeaking:function(r){return ga(t)(iI.value)(hI)(r)}}};var EI={q:1},pl=function(t){return{toInitializeNotch:function(r){return ga(t)(cI.value)(EI)(r)}}};var TI={gain:0},eE=function(t){return{toInitializeLowshelf:function(r){return ga(t)(fI.value)(TI)(r)}}};var SI={q:1},bd=function(t){return{toInitializeLowpass:function(r){return ga(t)(lI.value)(SI)(r)}}},lm={toInitializeLowpass:function(){var t=pd(bd(Et(ht()(J(Ct)(od)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()},xI=function(){return{loopStart:0,loopEnd:0,playbackRate:1,duration:W.value}}(),cf=function(t){return{toInitializeLoopBuf:function(r){return ga(t)(_I.value)(xI)(r)}}},Er={toInitializeLoopBuf:function(){var t=sd(cf(Et(ht()(J(Ct)(of)()()()({reflectSymbol:function(){return"buffer"}})))(gt()())));return function(r){return t(function(e){return{buffer:e}}(r))}}()},FI={gain:0},nE=function(t){return{toInitializeHighshelf:function(r){return ga(t)(pI.value)(FI)(r)}}};var OI={q:1},yd=function(t){return{toInitializeHighpass:function(r){return ga(t)(sI.value)(OI)(r)}}},vu={toInitializeHighpass:function(){var t=md(yd(Et(ht()(J(Ct)(id)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()},$I=function(){return{ratio:12,attack:.003,release:.25,knee:30,threshold:-24}}(),aE=function(t){return{toInitializeDynamicsCompressor:function(r){return ga(t)(mI.value)($I)(r)}}},wI={maxDelayTime:1},Ad=function(t){return{toInitializeDelay:function(r){return ga(t)(vI.value)(wI)(r)}}},un={toInitializeDelay:function(){var t=vd(Ad(Et(ht()(J(Ct)(cd)()()()({reflectSymbol:function(){return"delayTime"}})))(gt()())));return function(r){return t(function(e){return{delayTime:e}}(r))}}()},MI={q:1},dn=function(t){return{toInitializeBandpass:function(r){return ga(t)(DI.value)(MI)(r)}}},kd={toInitializeBandpass:function(){var t=Dd(dn(Et(ht()(J(Ct)(Dn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()},PI={q:1},_m=function(t){return{toInitializeAllpass:function(r){return ga(t)(dI.value)(PI)(r)}}},gd={toInitializeAllpass:function(){var t=dd(_m(Et(ht()(J(Ct)(fm)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()};var ci=function(){function t(){this.head=null,this.last=null,this.size=0}function r(f,m){this.queue=f,this.value=m,this.next=null,this.prev=null}function e(f){this.draining=!1,this.error=null,this.value=f,this.takes=new t,this.reads=new t,this.puts=new t}var n={};function a(f){try{f()}catch(m){setTimeout(function(){throw m},0)}}function u(f,m){var v=new r(f,m);switch(f.size){case 0:f.head=v;break;case 1:v.prev=f.head,f.head.next=v,f.last=v;break;default:v.prev=f.last,f.last.next=v,f.last=v}return f.size++,v}function i(f){var m;switch(f.size){case 0:return null;case 1:m=f.head,f.head=null;break;case 2:m=f.last,f.head.next=null,f.last=null;break;default:m=f.last,f.last=m.prev,f.last.next=null}return m.prev=null,m.queue=null,f.size--,m.value}function o(f){var m;switch(f.size){case 0:return null;case 1:m=f.head,f.head=null;break;case 2:m=f.head,f.last.prev=null,f.head=f.last,f.last=null;break;default:m=f.head,f.head=m.next,f.head.prev=null}return m.next=null,m.queue=null,f.size--,m.value}function p(f){if(f.queue!==null){if(f.queue.last===f){i(f.queue);return}if(f.queue.head===f){o(f.queue);return}f.prev&&(f.prev.next=f.next),f.next&&(f.next.prev=f.prev),f.queue.size--,f.queue=null,f.value=null,f.next=null,f.prev=null}}function s(f,m){if(!m.draining){var v=m.puts,c=m.takes,h=m.reads,ut,dt,fr,Zt,Kr;for(m.draining=!0;;){if(ut=null,dt=null,fr=null,Zt=m.value,Kr=h.size,m.error!==null){for(Zt=f.left(m.error);ut=o(v);)a(ut.cb(Zt));for(;dt=o(h);)a(dt(Zt));for(;fr=o(c);)a(fr(Zt));break}if(Zt===n&&(ut=o(v))&&(m.value=Zt=ut.value),Zt!==n){for(fr=o(c);Kr--&&(dt=o(h));)a(dt(f.right(Zt)));fr!==null&&(m.value=n,a(fr(f.right(Zt))))}if(ut!==null&&a(ut.cb(f.right(void 0))),m.value===n&&v.size===0||m.value!==n&&c.size===0)break}m.draining=!1}}return e.EMPTY=n,e.putLast=u,e.takeLast=i,e.takeHead=o,e.deleteCell=p,e.drainVar=s,e}();function rp(){return new ci(ci.EMPTY)}function uE(t,r,e){return function(){var n=ci.putLast(r.takes,e);return ci.drainVar(t,r),function(){ci.deleteCell(n)}}}function oE(t,r,e){return function(){return e.value===ci.EMPTY&&e.error===null?(e.value=r,ci.drainVar(t,e),!0):!1}}function iE(t,r){return function(){var e=r.value;return e===ci.EMPTY?t.nothing:(r.value=ci.EMPTY,ci.drainVar(t,r),t.just(e))}}var LI=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),BI=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),HI=function(){function t(){}return t.value=new t,t}();var Cd=function(){return{left:Kt.create,right:Yt.create,nothing:W.value,just:F.create,killed:LI.create,filled:BI.create,empty:HI.value}}();var cE=function(t){return function(r){return uE(Cd,t,r)}},pm=function(t){return function(r){return oE(Cd,t,r)}};var fE=function(t){return iE(Cd,t)};var UI=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}};var WI=function(){function t(){}return t.value=new t,t}();var sm={convertOption:function(t){return function(r){return rt(nt)}}},mm={convertOption:function(t){return function(r){return rt(nt)}}};var qI=function(t){return t.toInitializeAnalyser},Be=function(t){if(t instanceof y_)return W.value;if(t instanceof Ko)return new F(t.value0);throw new Error("Failed pattern match at Ocarina.Control (line 39, column 1 - line 39, column 38): "+[t.constructor.name])},Du=Jk(G)({doLogic:wc,ids:function(){var t=Te();return function(r){return function(e){return e.ids}(t(r))}}(),disconnectElement:function(t){return function(r){return t.disconnectXFromY({from:r.id,to:r.parent})}},toElt:function(t){return t}})({fromElt:Jn(),connectToParent:function(t){return function(r){return t.connectXToY({from:r.id,to:r.parent})}}});var ZI=function(){return{cb:function(t){return l(w)(l(w)(void 0))},fftSize:td.value,maxDecibels:-30,minDecibels:-100,smoothingTimeConstant:.8,channelCount:2,channelCountMode:ed.value,channelInterpretation:rd.value}}(),vm=function(t){return{toInitializeAnalyser:function(r){return ga(t)(WI.value)(ZI)(r)}}};var zI=function(t){return function(r){var e=Gh(t)(r),n=function(a){return function(u){return Ut(function(i){return function(){var p=u.ids();return a.raiseId(p)(),_(S)(function(s){return Q(ot)(i(u.deleteFromCache({id:p})))(s)})(Mt($t)(i)(l(C(G))(u.makeMicrophone({id:p,parent:a.parent,scope:Be(a.scope),microphone:e.microphone}))))()}})}};return new M(n)}},ep=function(t){return zI(t)};var na=Yu(w)(G)({doLogic:wc,ids:function(){var t=Te();return function(r){return function(e){return e.ids}(t(r))}}(),disconnectElement:function(t){return function(r){return t.disconnectXFromY({from:r.id,to:r.parent})}},toElt:function(t){return t}}),VI=function(t){return function(r){return function(e){return function(n){var a=qI(t)(r),u=function(i){return function(o){return Ut(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(I(R(w))(l(C(G))(o.makeAnalyser({id:f,parent:i.parent,scope:Be(i.scope),cb:a.cb,fftSize:pD(2)(function(){if(a.fftSize instanceof em)return 7;if(a.fftSize instanceof nm)return 8;if(a.fftSize instanceof XC)return 9;if(a.fftSize instanceof QC)return 10;if(a.fftSize instanceof td)return 11;if(a.fftSize instanceof KC)return 12;if(a.fftSize instanceof YC)return 13;throw new Error("Failed pattern match at Ocarina.Control (line 193, column 21 - line 200, column 34): "+[a.fftSize.constructor.name])}()),maxDecibels:a.maxDecibels,minDecibels:a.minDecibels,smoothingTimeConstant:a.smoothingTimeConstant,channelCount:a.channelCount,channelCountMode:function(){if(a.channelCountMode instanceof ph)return"explicit";if(a.channelCountMode instanceof ed)return"max";if(a.channelCountMode instanceof _h)return"clamped-max";throw new Error("Failed pattern match at Ocarina.Control (line 206, column 35 - line 209, column 46): "+[a.channelCountMode.constructor.name])}(),channelInterpretation:function(){if(a.channelInterpretation instanceof rd)return"speakers";if(a.channelInterpretation instanceof lh)return"discrete";throw new Error("Failed pattern match at Ocarina.Control (line 210, column 40 - line 212, column 41): "+[a.channelInterpretation.constructor.name])}()})))(I(R(w))(_(g)(function(m){return Ge()()()({cb:function(v){return o.setAnalyserNodeCb({id:f,cb:v})}})(m)})(e))(na({parent:new F(f),scope:i.scope,raiseId:Pt(rn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},Dm=function(t){return function(r){return VI(t)(r)(O(E(w)))}},_E=function(t){return function(r){return function(e){var n=Yh(t)(r),a=function(u){return function(i){return Ut(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(S)(function(f){return Q(ot)(o(i.deleteFromCache({id:s})))(f)})(Mt($t)(o)(I(R(w))(l(C(G))(i.makeConvolver({id:s,parent:u.parent,scope:Be(u.scope),buffer:n.buffer})))(na({parent:new F(s),scope:u.scope,raiseId:Pt(rn(Le(Ne)))})(i)(U(e)))))()}})}};return new M(a)}}},GI=function(){return function(){return function(t){return function(r){return function(e){return function(n){return function(a){var u=jh(t)(n)(r)(e),i=function(o){return function(p){return Ut(function(s){return function(){var m=p.ids();return o.raiseId(m)(),_(S)(function(v){return Q(ot)(s(p.deleteFromCache({id:m})))(v)})(Mt($t)(s)(I(R(w))(l(C(G))(p.makeIIRFilter({id:m,parent:o.parent,scope:Be(o.scope),feedforward:Gc()(u.feedforward),feedback:Gc()(u.feedback)})))(na({parent:new F(m),scope:o.scope,raiseId:Pt(rn(Le(Ne)))})(p)(U(a)))))()}})}};return new M(i)}}}}}}},pE=function(){return function(){return function(t){return GI()()(t)(d.value)(d.value)}}},hd=function(t){return function(r){return function(e){var n=qh(t)(r),a=function(u){return function(i){return Ut(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(S)(function(f){return Q(ot)(o(i.deleteFromCache({id:s})))(f)})(Mt($t)(o)(I(R(w))(l(C(G))(i.makeRecorder({id:s,parent:u.parent,scope:Be(u.scope),cb:n.cb})))(na({parent:new F(s),scope:u.scope,raiseId:Pt(rn(Le(Ne)))})(i)(e))))()}})}};return new M(a)}}},JI=function(t){return function(r){return Ut(function(e){return function(){var a=r.ids();return e(r.makeSpeaker({id:a}))(),$t(na({parent:new F(a),scope:new Ko("toplevel"),raiseId:Pt(rn(Le(Ne)))})(r)(U(t)))(e)()}})}},ff=JI,Nt=function(t){return function(r){return function(e){return Je(t)(r)(O(E(w)))(e)}}},Je=function(t){return function(r){return function(e){return function(n){var a=Qh(t)(r),u=function(i){return function(o){return Ut(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(I(R(w))(l(C(G))(o.makeGain({id:f,parent:i.parent,scope:Be(i.scope),gain:a.gain})))(I(R(w))(Ce(Vt(G))(_(g)(function(m){return Ge()()()({gain:sE(595)(i.scope)(o)(function(v){return o.setGain(function(c){return{id:f,gain:c}}(v))})})(m)})(e)))(na({parent:new F(f),scope:i.scope,raiseId:Pt(rn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},sE=UI("tmpResolveAU","Ocarina.Control",function(){var t=function(){var i=le()({reflectSymbol:function(){return"unit"}})(d.value);return function(o){return nl(i(o))}}(),r=function(){var i=le()({reflectSymbol:function(){return"sudden"}})(d.value);return function(o){return nl(i(o))}}(),e=function(){var i=le()({reflectSymbol:function(){return"numeric"}})(d.value);return function(o){return nl(i(o))}}(),n=function(){var i=le()({reflectSymbol:function(){return"envelope"}})(d.value);return function(o){return nl(i(o))}}(),a=function(){var i=le()({reflectSymbol:function(){return"cancel"}})(d.value);return function(o){return nl(i(o))}}(),u=function(i){return function(o){return function(p){return function(s){return Ge()()()({numeric:function(){var f=l(C(G));return function(m){return f(p(e(m)))}}(),envelope:function(){var f=l(C(G));return function(m){return f(p(n(m)))}}(),cancel:function(){var f=l(C(G));return function(m){return f(p(a(m)))}}(),sudden:function(){var f=l(C(G));return function(m){return f(p(r(m)))}}(),unit:function(f){var m=Nt(lt)(1)([f.u]);return Ut(function(v){return function(){var h=rp();return $t(I(R(w))(na({parent:W.value,scope:i,raiseId:function(ut){return cr(S)(pm(ut)(h))}})(o)(m))(Ut(function(ut){return function(){return cr(S)(cE(h)(function(fr){if(fr instanceof Kt)return Qc(fr.value0);if(fr instanceof Yt)return ut(p(t({i:fr.value0})));throw new Error("Failed pattern match at Ocarina.Control (line 1678, column 39 - line 1681, column 66): "+[fr.constructor.name])}))(),l(w)(void 0)}})))(v)()}})}})(s)}}}};return u}),me=sE(1657),jI=function(t){return function(r){return function(e){var n=sd(t)(r),a=function(u){return function(i){return Ut(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(S)(function(f){return Q(ot)(o(i.deleteFromCache({id:s})))(f)})(Mt($t)(o)(I(R(w))(l(C(G))(i.makeLoopBuf({id:s,parent:u.parent,scope:Be(u.scope),buffer:n.buffer,playbackRate:n.playbackRate,loopStart:n.loopStart,loopEnd:n.loopEnd,duration:n.duration})))(Ce(Vt(G))(_(g)(function(f){return Ge()()()({buffer:function(m){return l(C(G))(i.setBuffer({id:s,buffer:m}))},playbackRate:me(u.scope)(i)(function(m){return i.setPlaybackRate(function(v){return{id:s,playbackRate:v}}(m))}),loopStart:function(m){return l(C(G))(i.setLoopStart({id:s,loopStart:m}))},loopEnd:function(m){return l(C(G))(i.setLoopEnd({id:s,loopEnd:m}))},onOff:function(m){return l(C(G))(i.setOnOff({id:s,onOff:m}))}})(f)})(e)))))()}})}};return new M(a)}}},sr=function(t){return jI(t)};var XI=function(t){return function(r){return function(e){var n=Zh(t)(r),a=function(u){return function(i){return Ut(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(S)(function(f){return Q(ot)(o(i.deleteFromCache({id:s})))(f)})(Mt($t)(o)(I(R(w))(l(C(G))(i.makePeriodicOsc({id:s,parent:u.parent,scope:Be(u.scope),frequency:n.frequency,spec:n.spec})))(Ce(Vt(G))(_(g)(function(f){return Ge()()()({frequency:me(u.scope)(i)(function(m){return i.setFrequency(function(v){return{id:s,frequency:v}}(m))}),onOff:function(m){return l(C(G))(i.setOnOff({id:s,onOff:m}))},spec:function(m){return l(C(G))(i.setPeriodicOsc({id:s,spec:m}))}})(f)})(e)))))()}})}};return new M(a)}}},Bi=function(t){return XI(t)};var QI=function(t){return function(r){return function(e){var n=_d(t)(r),a=function(u){return function(i){return Ut(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(S)(function(f){return Q(ot)(o(i.deleteFromCache({id:s})))(f)})(Mt($t)(o)(I(R(w))(l(C(G))(i.makePlayBuf({id:s,parent:u.parent,scope:Be(u.scope),buffer:n.buffer,playbackRate:n.playbackRate,bufferOffset:n.bufferOffset,duration:n.duration})))(Ce(Vt(G))(_(g)(function(f){return Ge()()()({buffer:function(m){return l(C(G))(i.setBuffer({id:s,buffer:m}))},playbackRate:me(u.scope)(i)(function(m){return i.setPlaybackRate(function(v){return{id:s,playbackRate:v}}(m))}),bufferOffset:function(m){return l(C(G))(i.setBufferOffset({id:s,bufferOffset:m}))},onOff:function(m){return l(C(G))(i.setOnOff({id:s,onOff:m}))},duration:function(m){return l(C(G))(i.setDuration({id:s,duration:m}))}})(f)})(e)))))()}})}};return new M(a)}}},aa=function(t){return QI(t)};var KI=function(t){return function(r){return function(e){var n=Wh(t)(r),a=function(u){return function(i){return Ut(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(S)(function(f){return Q(ot)(o(i.deleteFromCache({id:s})))(f)})(Mt($t)(o)(I(R(w))(l(C(G))(i.makeSawtoothOsc({id:s,parent:u.parent,scope:Be(u.scope),frequency:n.frequency})))(Ce(Vt(G))(_(g)(function(f){return Ge()()()({frequency:me(u.scope)(i)(function(m){return i.setFrequency(function(v){return{id:s,frequency:v}}(m))}),onOff:function(m){return l(C(G))(i.setOnOff({id:s,onOff:m}))}})(f)})(e)))))()}})}};return new M(a)}}},mE=function(t){return KI(t)};var YI=function(t){return function(r){return function(e){var n=Uh(t)(r),a=function(u){return function(i){return Ut(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(S)(function(f){return Q(ot)(o(i.deleteFromCache({id:s})))(f)})(Mt($t)(o)(I(R(w))(l(C(G))(i.makeSinOsc({id:s,parent:u.parent,scope:Be(u.scope),frequency:n.frequency})))(Ce(Vt(G))(_(g)(function(f){return Ge()()()({frequency:me(u.scope)(i)(function(m){return i.setFrequency(function(v){return{id:s,frequency:v}}(m))}),onOff:function(m){return l(C(G))(i.setOnOff({id:s,onOff:m}))}})(f)})(e)))))()}})}};return new M(a)}}},lf=function(t){return YI(t)},vE=function(t){return function(r){return lf(t)(r)(O(E(w)))}},tR=function(t){return function(r){return function(e){var n=Hh(t)(r),a=function(u){return function(i){return Ut(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(S)(function(f){return Q(ot)(o(i.deleteFromCache({id:s})))(f)})(Mt($t)(o)(I(R(w))(l(C(G))(i.makeSquareOsc({id:s,parent:u.parent,scope:Be(u.scope),frequency:n.frequency})))(Ce(Vt(G))(_(g)(function(f){return Ge()()()({frequency:me(u.scope)(i)(function(m){return i.setFrequency(function(v){return{id:s,frequency:v}}(m))}),onOff:function(m){return l(C(G))(i.setOnOff({id:s,onOff:m}))}})(f)})(e)))))()}})}};return new M(a)}}},np=function(t){return tR(t)},DE=function(t){return function(r){return np(t)(r)(O(E(w)))}},rR=function(t){return function(r){return function(e){var n=Lh(t)(r),a=function(u){return function(i){return Ut(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(S)(function(f){return Q(ot)(o(i.deleteFromCache({id:s})))(f)})(Mt($t)(o)(I(R(w))(l(C(G))(i.makeTriangleOsc({id:s,parent:u.parent,scope:Be(u.scope),frequency:n.frequency})))(Ce(Vt(G))(_(g)(function(f){return Ge()()()({frequency:me(u.scope)(i)(function(m){return i.setFrequency(function(v){return{id:s,frequency:v}}(m))}),onOff:function(m){return l(C(G))(i.setOnOff({id:s,onOff:m}))}})(f)})(e)))))()}})}};return new M(a)}}},dm=function(t){return rR(t)};var eR=function(t){return function(r){return function(e){return function(n){var a=dd(t)(r),u=function(i){return function(o){return Ut(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(I(R(w))(l(C(G))(o.makeAllpass({id:f,parent:i.parent,scope:Be(i.scope),frequency:a.frequency,q:a.q})))(I(R(w))(Ce(Vt(G))(_(g)(function(m){return Ge()()()({frequency:me(i.scope)(o)(function(v){return o.setFrequency(function(c){return{id:f,frequency:c}}(v))}),q:me(i.scope)(o)(function(v){return o.setQ(function(c){return{id:f,q:c}}(v))})})(m)})(e)))(na({parent:new F(f),scope:i.scope,raiseId:Pt(rn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},ap=function(t){return function(r){return function(e){return eR(t)(r)(O(E(w)))(e)}}},Ed=function(t){return function(r){return function(e){return function(n){var a=Dd(t)(r),u=function(i){return function(o){return Ut(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(I(R(w))(l(C(G))(o.makeBandpass({id:f,parent:i.parent,scope:Be(i.scope),frequency:a.frequency,q:a.q})))(I(R(w))(Ce(Vt(G))(_(g)(function(m){return Ge()()()({frequency:me(i.scope)(o)(function(v){return o.setFrequency(function(c){return{id:f,frequency:c}}(v))}),q:me(i.scope)(o)(function(v){return o.setQ(function(c){return{id:f,q:c}}(v))})})(m)})(e)))(na({parent:new F(f),scope:i.scope,raiseId:Pt(rn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},wn=function(t){return function(r){return function(e){return Ed(t)(r)(O(E(w)))(e)}}},up=function(t){return function(r){return function(e){return function(n){var a=vd(t)(r),u=function(i){return function(o){return Ut(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(I(R(w))(l(C(G))(o.makeDelay({id:f,parent:i.parent,scope:Be(i.scope),delayTime:a.delayTime,maxDelayTime:a.maxDelayTime})))(I(R(w))(Ce(Vt(G))(_(g)(function(m){return Ge()()()({delayTime:me(i.scope)(o)(function(v){return o.setDelay(function(c){return{id:f,delayTime:c}}(v))})})(m)})(e)))(na({parent:new F(f),scope:i.scope,raiseId:Pt(rn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},Io=function(t){return function(r){return function(e){return up(t)(r)(O(E(w)))(e)}}},nR=function(t){return function(r){return function(e){return function(n){var a=Kh(t)(r),u=function(i){return function(o){return Ut(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(I(R(w))(l(C(G))(o.makeDynamicsCompressor({id:f,parent:i.parent,scope:Be(i.scope),threshold:a.threshold,ratio:a.ratio,knee:a.knee,attack:a.attack,release:a.release})))(I(R(w))(Ce(Vt(G))(_(g)(function(m){return Ge()()()({threshold:me(i.scope)(o)(function(v){return o.setThreshold(function(c){return{id:f,threshold:c}}(v))}),ratio:me(i.scope)(o)(function(v){return o.setRatio(function(c){return{id:f,ratio:c}}(v))}),knee:me(i.scope)(o)(function(v){return o.setKnee(function(c){return{id:f,knee:c}}(v))}),attack:me(i.scope)(o)(function(v){return o.setAttack(function(c){return{id:f,attack:c}}(v))}),release:me(i.scope)(o)(function(v){return o.setRelease(function(c){return{id:f,release:c}}(v))})})(m)})(e)))(na({parent:new F(f),scope:i.scope,raiseId:Pt(rn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},dE=function(t){return function(r){return nR(t)(r)(O(E(w)))}},aR=function(){return function(t){return function(r){return eD()(G)({doLogic:wc,ids:function(){var e=Te();return function(n){return function(a){return a.ids}(e(n))}}(),disconnectElement:function(e){return function(n){return e.disconnectXFromY({from:n.id,to:n.parent})}},toElt:function(e){return e}})({fromEltO1:Jn(),fromEltO2:Jn(),toElt:Jn(),wrapElt:function(e){return Nt(lt)(1)([e])},giveNewParent:function(e){return function(n){return function(a){return e.connectXToY({from:n.id,to:n.parent})}}},deleteFromCache:function(){var e=Te();return function(n){return function(a){return a.deleteFromCache}(e(n))}}()})(t)(ka(en)(_(k_)(function(e){return e(void 0)}))(Jn()(r)))}}},Wa=function(t){return function(r){return aR()(Ik(t))(ka(en)(Rk()()()()()({reflectType:function(){return 0}})(d.value))(r))}};var Td=function(t){return function(r){return function(e){return function(n){var a=md(t)(r),u=function(i){return function(o){return Ut(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(I(R(w))(l(C(G))(o.makeHighpass({id:f,parent:i.parent,scope:Be(i.scope),frequency:a.frequency,q:a.q})))(I(R(w))(Ce(Vt(G))(_(g)(function(m){return Ge()()()({frequency:me(i.scope)(o)(function(v){return o.setFrequency(function(c){return{id:f,frequency:c}}(v))}),q:me(i.scope)(o)(function(v){return o.setQ(function(c){return{id:f,q:c}}(v))})})(m)})(e)))(na({parent:new F(f),scope:i.scope,raiseId:Pt(rn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},sl=function(t){return function(r){return function(e){return Td(t)(r)(O(E(w)))(e)}}},uR=function(t){return function(r){return function(e){return function(n){var a=Xh(t)(r),u=function(i){return function(o){return Ut(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(I(R(w))(l(C(G))(o.makeHighshelf({id:f,parent:i.parent,scope:Be(i.scope),frequency:a.frequency,gain:a.gain})))(I(R(w))(Ce(Vt(G))(_(g)(function(m){return Ge()()()({frequency:me(i.scope)(o)(function(v){return o.setFrequency(function(c){return{id:f,frequency:c}}(v))}),gain:me(i.scope)(o)(function(v){return o.setGain(function(c){return{id:f,gain:c}}(v))})})(m)})(e)))(na({parent:new F(f),scope:i.scope,raiseId:Pt(rn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},bE=function(t){return function(r){return function(e){return uR(t)(r)(O(E(w)))(e)}}},yE=function(t){return function(r){return function(e){return function(n){var a=pd(t)(r),u=function(i){return function(o){return Ut(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(I(R(w))(l(C(G))(o.makeLowpass({id:f,parent:i.parent,scope:Be(i.scope),frequency:a.frequency,q:a.q})))(I(R(w))(Ce(Vt(G))(_(g)(function(m){return Ge()()()({frequency:me(i.scope)(o)(function(v){return o.setFrequency(function(c){return{id:f,frequency:c}}(v))}),q:me(i.scope)(o)(function(v){return o.setQ(function(c){return{id:f,q:c}}(v))})})(m)})(e)))(na({parent:new F(f),scope:i.scope,raiseId:Pt(rn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},ml=function(t){return function(r){return function(e){return yE(t)(r)(O(E(w)))(e)}}},oR=function(t){return function(r){return function(e){return function(n){var a=Jh(t)(r),u=function(i){return function(o){return Ut(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(I(R(w))(l(C(G))(o.makeLowshelf({id:f,parent:i.parent,scope:Be(i.scope),frequency:a.frequency,gain:a.gain})))(I(R(w))(Ce(Vt(G))(_(g)(function(m){return Ge()()()({frequency:me(i.scope)(o)(function(v){return o.setFrequency(function(c){return{id:f,frequency:c}}(v))}),gain:me(i.scope)(o)(function(v){return o.setGain(function(c){return{id:f,gain:c}}(v))})})(m)})(e)))(na({parent:new F(f),scope:i.scope,raiseId:Pt(rn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},AE=function(t){return function(r){return function(e){return oR(t)(r)(O(E(w)))(e)}}},iR=function(t){return function(r){return function(e){return function(n){var a=Vh(t)(r),u=function(i){return function(o){return Ut(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(I(R(w))(l(C(G))(o.makeNotch({id:f,parent:i.parent,scope:Be(i.scope),frequency:a.frequency,q:a.q})))(I(R(w))(Ce(Vt(G))(_(g)(function(m){return Ge()()()({frequency:me(i.scope)(o)(function(v){return o.setFrequency(function(c){return{id:f,frequency:c}}(v))}),q:me(i.scope)(o)(function(v){return o.setQ(function(c){return{id:f,q:c}}(v))})})(m)})(e)))(na({parent:new F(f),scope:i.scope,raiseId:Pt(rn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},vl=function(t){return function(r){return function(e){return iR(t)(r)(O(E(w)))(e)}}},cR=function(t){return function(r){return function(e){return function(n){var a=Bh(t)(r),u=function(i){return function(o){return Ut(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(I(R(w))(l(C(G))(o.makeStereoPanner({id:f,parent:i.parent,scope:Be(i.scope),pan:a.pan})))(I(R(w))(Ce(Vt(G))(_(g)(function(m){return Ge()()()({pan:me(i.scope)(o)(function(v){return o.setPan(function(c){return{id:f,pan:c}}(v))})})(m)})(e)))(na({parent:new F(f),scope:i.scope,raiseId:Pt(rn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},kE=function(t){return function(r){return cR(t)(r)(O(E(w)))}},fR=function(t){return function(r){return function(e){return function(n){var a=zh(t)(r),u=function(i){return function(o){return Ut(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(I(R(w))(l(C(G))(o.makePeaking({id:f,parent:i.parent,scope:Be(i.scope),frequency:a.frequency,q:a.q,gain:a.gain})))(I(R(w))(Ce(Vt(G))(_(g)(function(m){return Ge()()()({frequency:me(i.scope)(o)(function(v){return o.setFrequency(function(c){return{id:f,frequency:c}}(v))}),q:me(i.scope)(o)(function(v){return o.setQ(function(c){return{id:f,q:c}}(v))}),gain:me(i.scope)(o)(function(v){return o.setGain(function(c){return{id:f,gain:c}}(v))})})(m)})(e)))(na({parent:new F(f),scope:i.scope,raiseId:Pt(rn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},Dl=function(t){return function(r){return function(e){return fR(t)(r)(O(E(w)))(e)}}},gE=function(t){return function(r){return function(e){var n=ld(t)(r),a=function(u){return function(i){return Ut(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(S)(function(f){return Q(ot)(o(i.deleteFromCache({id:s})))(f)})(Mt($t)(o)(I(R(w))(l(C(G))(i.makeWaveShaper({id:s,parent:u.parent,scope:Be(u.scope),curve:n.curve,oversample:n.oversample})))(na({parent:new F(s),scope:u.scope,raiseId:Pt(rn(Le(Ne)))})(i)(U(e)))))()}})}};return new M(a)}}},lR=function(t){return function(r){return function(e){var n=tE(t)(r),a=function(u){return function(i){return Ut(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(S)(function(f){return Q(ot)(o(i.deleteFromCache({id:s})))(f)})(Mt($t)(o)(I(R(w))(l(C(G))(i.makeConstant({id:s,parent:u.parent,scope:Be(u.scope),offset:n.offset})))(Ce(Vt(G))(_(g)(function(f){return Ge()()()({offset:me(u.scope)(i)(function(m){return i.setOffset(function(v){return{id:s,offset:v}}(m))}),onOff:function(m){return l(C(G))(i.setOnOff({id:s,onOff:m}))}})(f)})(e)))))()}})}};return new M(a)}}},bm=function(t){return lR(t)};function Sd(){window.scrollTo(0,0)}var Ro=function(t){return t.sequential},Un=function(t){return t.parallel};var Mn=function(t){return function(r){return new M(X("button")(t)(U(r)))}};var qa=function(){var t={},r="Pure",e="Throw",n="Catch",a="Sync",u="Async",i="Bind",o="Bracket",p="Fork",s="Sequential",f="Map",m="Apply",v="Alt",c="Cons",h="Resume",ut="Release",dt="Finalizer",fr="Finalized",Zt="Forked",Kr="Fiber",re="Thunk";function pt(Gt,ee,Xe,ve){this.tag=Gt,this._1=ee,this._2=Xe,this._3=ve}function mr(Gt){var ee=function(Xe,ve,lr){return new pt(Gt,Xe,ve,lr)};return ee.tag=Gt,ee}function Fr(Gt){return new pt(r,void 0)}function ir(Gt){try{Gt()}catch(ee){setTimeout(function(){throw ee},0)}}function je(Gt,ee,Xe){try{return ee(Xe())}catch(ve){return Gt(ve)}}function Uo(Gt,ee,Xe){try{return ee(Xe)()}catch(ve){return Xe(Gt(ve))(),Fr}}var co=function(){var Gt=1024,ee=0,Xe=0,ve=new Array(Gt),lr=!1;function xt(){var ue;for(lr=!0;ee!==0;)ee--,ue=ve[Xe],ve[Xe]=void 0,Xe=(Xe+1)%Gt,ue();lr=!1}return{isDraining:function(){return lr},enqueue:function(ue){var Ir,Ze;ee===Gt&&(Ze=lr,xt(),lr=Ze),ve[(Xe+ee)%Gt]=ue,ee++,lr||xt()}}}();function Xi(Gt){var ee={},Xe=0,ve=0;return{register:function(lr){var xt=Xe++;lr.onComplete({rethrow:!0,handler:function(ue){return function(){ve--,delete ee[xt]}}})(),ee[xt]=lr,ve++},isEmpty:function(){return ve===0},killAll:function(lr,xt){return function(){if(ve===0)return xt();var ue=0,Ir={};function Ze(ke){Ir[ke]=ee[ke].kill(lr,function(on){return function(){delete Ir[ke],ue--,Gt.isLeft(on)&&Gt.fromLeft(on)&&setTimeout(function(){throw Gt.fromLeft(on)},0),ue===0&&xt()}})()}for(var ua in ee)ee.hasOwnProperty(ua)&&(ue++,Ze(ua));return ee={},Xe=0,ve=0,function(ke){return new pt(a,function(){for(var on in Ir)Ir.hasOwnProperty(on)&&Ir[on]()})}}}}}var hu=0,yn=1,vi=2,vf=3,Df=4,Pe=5,Di=6;function df(Gt,ee,Xe){var ve=0,lr=hu,xt=Xe,ue=null,Ir=null,Ze=null,ua=null,ke=null,on=0,$c=0,Uu=null,Qi=!0;function Ki(_r){for(var vr,Vr,Yr;;)switch(vr=null,Vr=null,Yr=null,lr){case vi:lr=yn;try{xt=Ze(xt),ua===null?Ze=null:(Ze=ua._1,ua=ua._2)}catch(da){lr=Pe,ue=Gt.left(da),xt=null}break;case vf:Gt.isLeft(xt)?(lr=Pe,ue=xt,xt=null):Ze===null?lr=Pe:(lr=vi,xt=Gt.fromRight(xt));break;case yn:switch(xt.tag){case i:Ze&&(ua=new pt(c,Ze,ua)),Ze=xt._2,lr=yn,xt=xt._1;break;case r:Ze===null?(lr=Pe,xt=Gt.right(xt._1)):(lr=vi,xt=xt._1);break;case a:lr=vf,xt=je(Gt.left,Gt.right,xt._1);break;case u:lr=Df,xt=Uo(Gt.left,xt._1,function(da){return function(){ve===_r&&(ve++,co.enqueue(function(){ve===_r+1&&(lr=vf,xt=da,Ki(ve))}))}});return;case e:lr=Pe,ue=Gt.left(xt._1),xt=null;break;case n:Ze===null?ke=new pt(c,xt,ke,Ir):ke=new pt(c,xt,new pt(c,new pt(h,Ze,ua),ke,Ir),Ir),Ze=null,ua=null,lr=yn,xt=xt._1;break;case o:on++,Ze===null?ke=new pt(c,xt,ke,Ir):ke=new pt(c,xt,new pt(c,new pt(h,Ze,ua),ke,Ir),Ir),Ze=null,ua=null,lr=yn,xt=xt._1;break;case p:lr=vf,vr=df(Gt,ee,xt._2),ee&&ee.register(vr),xt._1&&vr.run(),xt=Gt.right(vr);break;case s:lr=yn,xt=fS(Gt,ee,xt._1);break}break;case Pe:if(Ze=null,ua=null,ke===null)lr=Di,xt=Ir||ue||xt;else switch(vr=ke._3,Yr=ke._1,ke=ke._2,Yr.tag){case n:Ir&&Ir!==vr&&on===0?lr=Pe:ue&&(lr=yn,xt=Yr._2(Gt.fromLeft(ue)),ue=null);break;case h:Ir&&Ir!==vr&&on===0||ue?lr=Pe:(Ze=Yr._1,ua=Yr._2,lr=vi,xt=Gt.fromRight(xt));break;case o:on--,ue===null&&(Vr=Gt.fromRight(xt),ke=new pt(c,new pt(ut,Yr._2,Vr),ke,vr),(Ir===vr||on>0)&&(lr=yn,xt=Yr._3(Vr)));break;case ut:ke=new pt(c,new pt(fr,xt,ue),ke,Ir),lr=yn,Ir&&Ir!==vr&&on===0?xt=Yr._1.killed(Gt.fromLeft(Ir))(Yr._2):ue?xt=Yr._1.failed(Gt.fromLeft(ue))(Yr._2):xt=Yr._1.completed(Gt.fromRight(xt))(Yr._2),ue=null,on++;break;case dt:on++,ke=new pt(c,new pt(fr,xt,ue),ke,Ir),lr=yn,xt=Yr._1;break;case fr:on--,lr=Pe,xt=Yr._1,ue=Yr._2;break}break;case Di:for(var Ke in Uu)Uu.hasOwnProperty(Ke)&&(Qi=Qi&&Uu[Ke].rethrow,ir(Uu[Ke].handler(xt)));Uu=null,Ir&&ue?setTimeout(function(){throw Gt.fromLeft(ue)},0):Gt.isLeft(xt)&&Qi&&setTimeout(function(){if(Qi)throw Gt.fromLeft(xt)},0);return;case hu:lr=yn;break;case Df:return}}function Qe(_r){return function(){if(lr===Di)return Qi=Qi&&_r.rethrow,_r.handler(xt)(),function(){};var vr=$c++;return Uu=Uu||{},Uu[vr]=_r,function(){Uu!==null&&delete Uu[vr]}}}function yr(_r,vr){return function(){if(lr===Di)return vr(Gt.right(void 0))(),function(){};var Vr=Qe({rethrow:!1,handler:function(){return vr(Gt.right(void 0))}})();switch(lr){case hu:Ir=Gt.left(_r),lr=Di,xt=Ir,Ki(ve);break;case Df:Ir===null&&(Ir=Gt.left(_r)),on===0&&(lr===Df&&(ke=new pt(c,new pt(dt,xt(_r)),ke,Ir)),lr=Pe,xt=null,ue=null,Ki(++ve));break;default:Ir===null&&(Ir=Gt.left(_r)),on===0&&(lr=Pe,xt=null,ue=null)}return Vr}}function Lr(_r){return function(){var vr=Qe({rethrow:!1,handler:_r})();return lr===hu&&Ki(ve),vr}}return{kill:yr,join:Lr,onComplete:Qe,isSuspended:function(){return lr===hu},run:function(){lr===hu&&(co.isDraining()?Ki(ve):co.enqueue(function(){Ki(ve)}))}}}function di(Gt,ee,Xe,ve){var lr=0,xt={},ue=0,Ir={},Ze=new Error("[ParAff] Early exit"),ua=null,ke=t;function on(Qe,yr,Lr){var _r=yr,vr=null,Vr=null,Yr=0,Ke={},da,Rl;t:for(;;)switch(da=null,_r.tag){case Zt:if(_r._3===t&&(da=xt[_r._1],Ke[Yr++]=da.kill(Qe,function(lS){return function(){Yr--,Yr===0&&Lr(lS)()}})),vr===null)break t;_r=vr._2,Vr===null?vr=null:(vr=Vr._1,Vr=Vr._2);break;case f:_r=_r._2;break;case m:case v:vr&&(Vr=new pt(c,vr,Vr)),vr=_r,_r=_r._1;break}if(Yr===0)Lr(Gt.right(void 0))();else for(Rl=0,da=Yr;Rl<da;Rl++)Ke[Rl]=Ke[Rl]();return Ke}function $c(Qe,yr,Lr){var _r,vr,Vr,Yr,Ke,da;Gt.isLeft(Qe)?(_r=Qe,vr=null):(vr=Qe,_r=null);t:for(;;){if(Vr=null,Yr=null,Ke=null,da=null,ua!==null)return;if(yr===null){ve(_r||vr)();return}if(yr._3!==t)return;switch(yr.tag){case f:_r===null?(yr._3=Gt.right(yr._1(Gt.fromRight(vr))),vr=yr._3):yr._3=_r;break;case m:if(Vr=yr._1._3,Yr=yr._2._3,_r){if(yr._3=_r,Ke=!0,da=ue++,Ir[da]=on(Ze,_r===Vr?yr._2:yr._1,function(){return function(){delete Ir[da],Ke?Ke=!1:Lr===null?$c(_r,null,null):$c(_r,Lr._1,Lr._2)}}),Ke){Ke=!1;return}}else{if(Vr===t||Yr===t)return;vr=Gt.right(Gt.fromRight(Vr)(Gt.fromRight(Yr))),yr._3=vr}break;case v:if(Vr=yr._1._3,Yr=yr._2._3,Vr===t&&Gt.isLeft(Yr)||Yr===t&&Gt.isLeft(Vr))return;if(Vr!==t&&Gt.isLeft(Vr)&&Yr!==t&&Gt.isLeft(Yr))_r=vr===Vr?Yr:Vr,vr=null,yr._3=_r;else if(yr._3=vr,Ke=!0,da=ue++,Ir[da]=on(Ze,vr===Vr?yr._2:yr._1,function(){return function(){delete Ir[da],Ke?Ke=!1:Lr===null?$c(vr,null,null):$c(vr,Lr._1,Lr._2)}}),Ke){Ke=!1;return}break}Lr===null?yr=null:(yr=Lr._1,Lr=Lr._2)}}function Uu(Qe){return function(yr){return function(){delete xt[Qe._1],Qe._3=yr,$c(yr,Qe._2._1,Qe._2._2)}}}function Qi(){var Qe=yn,yr=Xe,Lr=null,_r=null,vr,Vr;t:for(;;)switch(vr=null,Vr=null,Qe){case yn:switch(yr.tag){case f:Lr&&(_r=new pt(c,Lr,_r)),Lr=new pt(f,yr._1,t,t),yr=yr._2;break;case m:Lr&&(_r=new pt(c,Lr,_r)),Lr=new pt(m,t,yr._2,t),yr=yr._1;break;case v:Lr&&(_r=new pt(c,Lr,_r)),Lr=new pt(v,t,yr._2,t),yr=yr._1;break;default:Vr=lr++,Qe=Pe,vr=yr,yr=new pt(Zt,Vr,new pt(c,Lr,_r),t),vr=df(Gt,ee,vr),vr.onComplete({rethrow:!1,handler:Uu(yr)})(),xt[Vr]=vr,ee&&ee.register(vr)}break;case Pe:if(Lr===null)break t;Lr._1===t?(Lr._1=yr,Qe=yn,yr=Lr._2,Lr._2=t):(Lr._2=yr,yr=Lr,_r===null?Lr=null:(Lr=_r._1,_r=_r._2))}for(ke=yr,Vr=0;Vr<lr;Vr++)xt[Vr].run()}function Ki(Qe,yr){ua=Gt.left(Qe);var Lr;for(var _r in Ir)if(Ir.hasOwnProperty(_r)){Lr=Ir[_r];for(_r in Lr)Lr.hasOwnProperty(_r)&&Lr[_r]()}Ir=null;var vr=on(Qe,ke,yr);return function(Vr){return new pt(u,function(Yr){return function(){for(var Ke in vr)vr.hasOwnProperty(Ke)&&vr[Ke]();return Fr}})}}return Qi(),function(Qe){return new pt(u,function(yr){return function(){return Ki(Qe,yr)}})}}function fS(Gt,ee,Xe){return new pt(u,function(ve){return function(){return di(Gt,ee,Xe,ve)}})}return pt.EMPTY=t,pt.Pure=mr(r),pt.Throw=mr(e),pt.Catch=mr(n),pt.Sync=mr(a),pt.Async=mr(u),pt.Bind=mr(i),pt.Bracket=mr(o),pt.Fork=mr(p),pt.Seq=mr(s),pt.ParMap=mr(f),pt.ParApply=mr(m),pt.ParAlt=mr(v),pt.Fiber=df,pt.Supervisor=Xi,pt.Scheduler=co,pt.nonCanceler=Fr,pt}(),CE=qa.Pure,bR=qa.Throw;function hE(t){return function(r){return r.tag===qa.Pure.tag?qa.Pure(t(r._1)):qa.Bind(r,function(e){return qa.Pure(t(e))})}}function EE(t){return function(r){return qa.Bind(t,r)}}var TE=qa.Sync;function SE(t){return function(r){return qa.ParMap(t,r)}}function xE(t){return function(r){return qa.ParApply(t,r)}}function FE(t){return function(r){return qa.ParAlt(t,r)}}var dl=qa.Async;function OE(t,r){return function(){return qa.Fiber(t,null,r)}}var yR=function(){function t(e,n){return e===0&&typeof setImmediate<"u"?setImmediate(n):setTimeout(n,e)}function r(e,n){return e===0&&typeof clearImmediate<"u"?clearImmediate(n):clearTimeout(n)}return function(e,n){return qa.Async(function(a){return function(){var u=t(n,a(e()));return function(){return qa.Sync(function(){return e(r(n,u))})}}})}}(),$E=qa.Seq;var kR=function(t){return function(r){return function(e){var n=Ro(t),a=ce(t.Applicative1())(r)(function(){var u=Un(t);return function(i){return u(e(i))}}());return function(u){return n(a(u))}}}},wE=function(t){return function(r){return function(e){var n=Ro(t),a=Nn(r)(t.Applicative1())(function(){var u=Un(t);return function(i){return u(e(i))}}());return function(u){return n(a(u))}}}},ME=function(t){return function(r){return kR(t)(r)(rt(nt))}};var gR=function(t){return t};var IE=function(t){return t};var ip=function(t){return t.toDuration};var RE={fromDuration:_v()()(gR)(function(t){return t*1e3}),toDuration:_v()()(IE)(function(t){return t/1e3})};var NE=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}};var hR=function(t){return t};var yl={map:SE},Hi={map:hE};var ER=function(){var t=function(n){if(n instanceof Yt)return n.value0;if(n instanceof Kt)return _u("unsafeFromRight: Left");throw new Error("Failed pattern match at Effect.Aff (line 407, column 21 - line 409, column 54): "+[n.constructor.name])},r=function(n){if(n instanceof Kt)return n.value0;if(n instanceof Yt)return _u("unsafeFromLeft: Right");throw new Error("Failed pattern match at Effect.Aff (line 402, column 20 - line 404, column 55): "+[n.constructor.name])},e=function(n){if(n instanceof Kt)return!0;if(n instanceof Yt)return!1;throw new Error("Failed pattern match at Effect.Aff (line 397, column 12 - line 399, column 21): "+[n.constructor.name])};return{isLeft:e,fromLeft:r,fromRight:t,left:Kt.create,right:Yt.create}}(),TR=function(t){return OE(ER,t)},No=function(t){return function(){var e=TR(t)();return e.run(),e}},li=function(){var t=cr(S);return function(r){return t(No(r))}}();var Ui={apply:xE,Functor0:function(){return yl}};var xd={Applicative0:function(){return xa},Bind1:function(){return Ue}},Ue={bind:EE,Apply0:function(){return Fd(0)}},xa={pure:CE,Apply0:function(){return Fd(0)}},Fd=NE("applyAff","Effect.Aff",function(){return{apply:_o(xd),Functor0:function(){return Hi}}}),LE=Fd(71);var He={liftEffect:TE,Monad0:function(){return xd}},BE=function(){var t=Ae(He);return function(r){return hR(T(t(r)))}}(),HE=function(t){return dl(function(r){return _(S)(BE)(t.join(r))})};var UE=function(t){return function(r){return P(Ue)(Ae(He)(r.isSuspended))(function(e){return e?Ae(He)(cr(S)(r.kill(t,T(l(w)(void 0))))):dl(function(n){return _(S)(BE)(r.kill(t,n))})})}};var Wn={parallel:mt,sequential:$E,Monad0:function(){return xd},Applicative1:function(){return SR(0)}},SR=NE("applicativeParAff","Effect.Aff",function(){return{pure:function(){var t=Un(Wn),r=l(xa);return function(e){return t(r(e))}}(),Apply0:function(){return Ui}}});var xR={append:function(t){return function(r){return function(e){return ME(Wn)(Bt)([t(e),r(e)])}}}};var FR=T(l(xa)(void 0)),WE={mempty:FR,Semigroup0:function(){return xR}};var qE={alt:FE,Functor0:function(){return yl}};var ZE=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),cp=function(){function t(){}return t.value=new t,t}(),_f=function(){function t(){}return t.value=new t,t}(),fp=function(){function t(){}return t.value=new t,t}(),pf=function(){function t(){}return t.value=new t,t}(),lp=function(){function t(){}return t.value=new t,t}(),_p=function(){function t(){}return t.value=new t,t}(),zE=function(){function t(){}return t.value=new t,t}(),ym=function(){function t(){}return t.value=new t,t}(),Am=function(){function t(){}return t.value=new t,t}(),pp=function(){function t(){}return t.value=new t,t}(),sp=function(){function t(){}return t.value=new t,t}(),VE=function(){function t(){}return t.value=new t,t}(),Al=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Od=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var OR="numeric",$R="sudden",wR="unit",MR="cancel",PR="step",IR="linear",RR="exponential",NR="envelope",GE=function(t,r,e,n){if(e.type===$R)t.value=e.value.n;else if(e.type===wR)r.id&&BR(r.id,n),n.units[e.value.i].main.connect(t),r.id=e.value.i;else if(e.type===OR)t[e.value.t.type===PR?"setValueAtTime":e.value.t.type===IR?"linearRampToValueAtTime":e.value.t.type===RR?"exponentialRampToValueAtTime":"linearRampToValueAtTime"](e.value.n,e.value.o);else if(e.type===MR)e.value.hold?t.cancelAndHoldAtTime(e.value.o):t.cancelScheduledValues(e.value.o);else if(e.type===NR){let a=e.value.o;t.cancelScheduledValues(Math.max(0,a)),t.setValueCurveAtTime(e.value.p,a,e.value.d)}else throw new Error("No idea what to do with "+JSON.stringify(e))},LR=function(t,r,e,n,a){return n[e]||(n[e]={}),GE(r.parameters.get(e),n[e],a,t)},uo=function(t,r,e,n,a){return n[e]||(n[e]={}),GE(r[e],n[e],a,t)},Fe=function(t,r,e,n){let a=t("@fan@")(u=>u)(e);n.scopes[a]||(n.scopes[a]=[]),n.scopes[a].push(r),n.units[r].scope=a},Oe=function(t,r){r.toConnect[t]&&(r.toConnect[t].forEach(function(e){e.w?r.units[e.w]?e.f():(r.toConnect[e.w]||(r.toConnect[e.w]=[]),r.toConnect[e.w].push({f:e.f})):e.f()}),delete r.toConnect[t])},$e=function(t,r,e,n){t()(a=>JE(r,a,n))(e)},JE=function(t,r,e){var n=function(){e.units[t].audioOutgoing.push(r),e.units[t].pendingOn||(e.units[t].main.connect(e.units[r].main),e.units[r].se&&e.units[t].main.connect(e.units[r].se))};if(!e.units[t]){e.toConnect[t]||(e.toConnect[t]=[]);var a={f:n};r!==t&&!e.units[r]&&(a.w=r),e.toConnect[t].push(a);return}if(!e.units[r]){e.toConnect[r]||(e.toConnect[r]=[]);var a={f:n};r!==t&&!e.units[t]&&(a.w=t),e.toConnect[r].push(a);return}n()};function $d(t){return function(r){return function(){delete r.units[t.id]}}}function wd(t){return function(r){return function(){JE(t.from,t.to,r)}}}var BR=function(t,r){if(r.units[t].scope==="@fan@")return;let e=r.units[t].scope;r.scopes[e].forEach(n=>{delete r.units[n]}),delete r.scopes[e]};function Md(t){return function(r){return function(){var e=t.from,n=t.to;if(!r.units[e]||(r.units[e].audioOutgoing=r.units[e].audioOutgoing.filter(function(u){return u!==n}),r.units[e].main.disconnect(r.units[n].main),r.units[n].se&&r.units[e].main.disconnect(r.units[n].se),r.units[e].scope==="@fan@"))return;let a=r.units[e].scope;r.scopes[a].forEach(u=>{delete r.units[u]}),delete r.scopes[a]}}}var Pd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"allpass",Q:r.q,frequency:r.frequency})},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Id=t=>r=>e=>()=>{var n=r.id,a=r.cb,u=new AnalyserNode(e.context,r),i=a(u)();e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],analyserOrig:a,analyser:i,main:e.context.createGain(),se:u},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Rd=t=>r=>e=>()=>{var n=r.id,a=r.options;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new AudioWorkletNode(e.context,a.name,{numberOfInputs:a.numberOfInputs,numberOfOutputs:a.numberOfOutputs,outputChannelCount:a.outputChannelCount,parameterData:a.parameterData,processorOptions:a.processorOptions})},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Nd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"bandpass",Q:r.q,frequency:r.frequency})},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Ld=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new ConstantSourceNode(i,o)},u={offset:r.offset};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Bd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new ConvolverNode(e.context,{buffer:r.buffer})},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Hd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DelayNode(e.context,{delayTime:r.delayTime,maxDelayTime:r.maxDelayTime})},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Ud=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DynamicsCompressorNode(e.context,{knee:r.knee,ratio:r.ratio,threshold:r.threshold,attack:r.attack,release:r.release})},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Wd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new GainNode(e.context,{gain:r.gain})},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},qd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highpass",Q:r.q,frequency:r.frequency})},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Zd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highshelf",frequency:r.frequency,gain:r.gain})},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},zd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new IIRFilterNode(e.context,{feedforward:r.feedforward,feedback:r.feedback})},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Vd=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new AudioBufferSourceNode(i,o)},u={loop:!0,buffer:r.buffer,loopStart:r.loopStart,loopEnd:r.loopEnd,playbackRate:r.playbackRate};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Gd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowpass",Q:r.q,frequency:r.frequency})},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Jd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowshelf",frequency:r.frequency,gain:r.gain})},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},jd=t=>r=>e=>()=>{var n=r.id,a=r.element,u=function(){var i=e.context.createMediaElementSource(a);return i};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],createClosure:u,resumeClosure:{},main:u()},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Xd=t=>r=>e=>()=>{var n=r.id;e.units[r.id]={main:e.context.createMediaStreamSource(r.microphone),controllers:{},audioOutgoing:[],controlOutgoing:[]},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Qd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"notch",frequency:r.frequency,Q:r.q})},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Kd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"peaking",frequency:r.frequency,Q:r.q,gain:r.gain})},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Yd=t=>r=>e=>()=>{var n=r.id,a=function(i,o){var p={frequency:o.frequency,periodicWave:o.spec.type==="wave"?o.spec.value:Nb(e.context)(o.spec.value.real)(o.spec.value.img)()},s=new OscillatorNode(i,p);return s},u={frequency:r.frequency,type:"custom",spec:r.spec};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},tb=t=>r=>e=>()=>{var n=r.id,a=function(i,o){var p={loop:o.loop,buffer:o.buffer,playbackRate:o.playbackRate};return new AudioBufferSourceNode(i,p)},u={loop:!1,buffer:r.buffer,playbackRate:r.playbackRate,bufferOffset:r.bufferOffset,duration:t(void 0)(i=>i)(r.duration)};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},rb=t=>r=>e=>()=>{var n=r.id,a=r.cb,u=e.context.createMediaStreamDestination(),i=new MediaRecorder(u.stream);a(i)(),i.start(),e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],recorderOrig:a,recorder:i,main:e.context.createGain(),se:u},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},eb=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new OscillatorNode(i,o)},u={frequency:r.frequency,type:"sawtooth"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},nb=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new OscillatorNode(i,o)},u={frequency:r.frequency,type:"sine"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},ab=t=>r=>()=>{r.units[t.id]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:r.context.createGain(),se:r.context.destination}},ub=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new StereoPannerNode(e.context,{pan:r.pan})},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},ob=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new OscillatorNode(i,o)},u={frequency:r.frequency,type:"square"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},ib=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new OscillatorNode(i,o)},u={frequency:r.frequency,type:"triangle"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},cb=t=>r=>e=>()=>{var n=r.id,a=r.curve,u=r.oversample;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new WaveShaperNode(e.context,{curve:a,oversample:u.type})},Fe(t,n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)};function fb(t){return function(r){return function(){var e=t.id,n=t.cb;r.units[e].analyserOrig!==n&&(r.units[e].analyser&&r.units[e].analyser(),r.units[e].analyser=n(r.units[e].se)(),r.units[e].analyserOrig=n)}}}function lb(t){return function(r){return function(){var e=t.cb,n=t.id;if(r.units[n].recorderOrig!==e){r.units[n].recorder&&r.units[n].recorder.stop();var a=e;r.units[n].recorderOrig=e;var u=new MediaRecorder(r.units[n].se);a(u)(),u.start()}}}}function _b(t){return function(r){return function(){var e=t.id,n=t.curve;r.units[e].main.curve=n}}}function pb(t){return function(r){return function(){var e=t.id,n=t.paramName,a=t.paramValue;LR(r,r.units[e].main,n,r.units[e].controllers,a)}}}var oo=function(t,r,e){r.resume&&t.value.n!==void 0&&(r.resume[e]=t.value.n)};function sb(t){return function(r){return function(){var e=t.id,n=t.gain;uo(r,r.units[e].main,"gain",r.units[e].controllers,n),oo(n,r.units[e],"gain")}}}function mb(t){return function(r){return function(){var e=t.id,n=t.q;uo(r,r.units[e].main,"Q",r.units[e].controllers,n),oo(n,r.units[e],"Q")}}}function vb(t){return function(r){return function(){var e=t.id,n=t.buffer;r.units[e].resume&&(r.units[e].resume.buffer=n)}}}function Db(t){return function(r){return function(){var e=t.id,n=t.buffer;r.units[e].main.buffer=n}}}function db(t){return function(r){return function(){var e=t.id,n=t.spec;r.units[e].resume&&(r.units[e].resume.spec=n)}}}function bb(t){return function(r){return function(){var e=t.id,n=t.pan;uo(r,r.units[e].main,"pan",r.units[e].controllers,n),oo(n,r.units[e],"pan")}}}function yb(t){return function(r){return function(){var e=t.id,n=t.threshold;uo(r,r.units[e].main,"threshold",r.units[e].controllers,n),oo(n,r.units[e],"threshold")}}}function Ab(t){return function(r){return function(){var e=t.id,n=t.loopStart;r.units[e].main.loopStart=n,r.units[e].resume.loopStart=n}}}function kb(t){return function(r){return function(){var e=t.id,n=t.loopEnd;r.units[e].main.loopEnd=n,r.units[e].resume.loopEnd=n}}}function gb(t){return function(r){return function(){var e=t.id,n=t.bufferOffset;r.units[e].resume.bufferOffset=n}}}function Cb(t){return function(r){return function(e){return function(){var n=r.id,a=r.duration;e.units[n].duration=t(void 0)(u=>u)(a)}}}}function hb(t){return function(r){return function(){var e=t.id,n=t.release;uo(r,r.units[e].main,"release",r.units[e].controllers,n),oo(n,r.units[e],"release")}}}function Eb(t){return function(r){return function(){var e=t.id,n=t.offset;uo(r,r.units[e].main,"offset",r.units[e].controllers,n),oo(n,r.units[e],"offset")}}}function Tb(t){return function(r){return function(){var e=t.id,n=t.ratio;uo(r,r.units[e].main,"ratio",r.units[e].controllers,n),oo(n,r.units[e],"ratio")}}}function Sb(t){return function(r){return function(){var e=t.id,n=t.attack;uo(r,r.units[e].main,"attack",r.units[e].controllers,n),oo(n,r.units[e],"attack")}}}function xb(t){return function(r){return function(){var e=t.id,n=t.knee;uo(r,r.units[e].main,"knee",r.units[e].controllers,n),oo(n,r.units[e],"knee")}}}function Fb(t){return function(r){return function(){var e=t.id,n=t.delayTime;uo(r,r.units[e].main,"delayTime",r.units[e].controllers,n),oo(n,r.units[e],"delayTime")}}}function Ob(t){return function(r){return function(){var e=t.id,n=t.playbackRate;uo(r,r.units[e].main,"playbackRate",r.units[e].controllers,n),oo(n,r.units[e],"playbackRate")}}}function $b(t){return function(r){return function(){var e=t.id,n=t.frequency;uo(r,r.units[e].main,"frequency",r.units[e].controllers,n),oo(n,r.units[e],"frequency")}}}function wb(t){return function(r){return function(){var e=t.id,n=t.onOff;n.x.type==="on"?HR(e)(n)(r)():n.x.type==="off"&&UR(e)(n)(r)()}}}var HR=function(t){return function(r){return function(e){return function(){if(!e.units[t].onOff){e.units[t].pendingOn=!1,e.units[t].onOff=!0,e.units[t].main=e.units[t].createClosure(e.context,e.units[t].resume);for(var n=0;n<e.units[t].audioOutgoing.length;n++){var a=e.units[t].audioOutgoing[n];e.units[t].main.connect(e.units[a].main),e.units[a].se&&e.units[t].main.connect(e.units[a].se)}e.units[t].resume&&e.units[t].resume.bufferOffset?typeof e.units[t].resume.duration=="number"?e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.bufferOffset,e.units[t].resume.duration):e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.bufferOffset):e.units[t].resume&&e.units[t].resume.loopStart?e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.loopStart):e.units[t].main.start(e.deprecatedWriteHead+r.o)}}}}},UR=function(t){return function(r){return function(e){return function(){if(!!e.units[t].onOff){e.units[t].onOff=!1;var n=e.units[t].main;n.addEventListener("ended",()=>{n.disconnect()}),n.stop(e.deprecatedWriteHead+r.o)}}}}};function Mb(t){for(var r=new Float32Array(t.length),e=0;e<t.length;e++)r[e]=t[e];return r}function km(t){return function(){t.stop()}}function Pb(t){return function(r){return function(e){return function(){var n=[];e.ondataavailable=function(a){n.push(a.data)},e.onstop=function(){var a=new Blob(n,{type:t});r(a)(),n=null}}}}}function Ib(t){return function(r){return function(){return navigator.mediaDevices.getUserMedia({audio:t,video:r})}}}function mp(t){return function(){var r=new Uint8Array(t.frequencyBinCount);return t.getByteFrequencyData(r),r}}function Rb(t){return function(){var r=t.createConstantSource();return r.offset.value=0,r.connect(t.destination),r.start(),function(){r.stop(),r.disconnect(t.destination)}}}var Nb=function(t){return function(r){return function(e){return function(){for(var n=new Float32Array(r.length),a=new Float32Array(e.length),u=0;u<r.length;u++)n[u]=r[u];for(var u=0;u<e.length;u++)a[u]=e[u];return t.createPeriodicWave(n,a,{disableNormalization:!0})}}}};function gc(t){return function(){return{context:t,deprecatedWriteHead:0,units:{},scopes:{},unsu:{},toConnect:{}}}}function Lb(t){return function(){t.close()}}function Bb(t){return function(){return fetch(t).then(function(r){return r.arrayBuffer()},function(r){return console.error("Error fetching buffer",r),Promise.reject(r)})}}function Hb(t){return function(r){return function(){return t.decodeAudioData(r)}}}function Ub(){return new(window.AudioContext||window.webkitAudioContext)}function Wb(t){return function(){return t.state}}function vp(t){return function(){return t.currentTime}}function jE(t){return function(r){return function(e){return function(){t.then(e,r)}}}}var ZR=function(t){return function(r){return dl(function(e){return yf(S)(Pt(WE))(jE(r)(function(n){return e(Kt.create(t(n)))()})(function(n){return e(Yt.create(n))()}))})}};var zR=function(t){return ja(function(r){return ai("Promise failed, couldn't extract JS Error or String")})(rt(nt))(VD(I(zD(Fv)(zu))(QD(zu)("Error")(t))(_(z_(so))(ai)(KD(zu)(t)))))},XE=ZR(zR),gm=function(t){return P(Ue)(Ae(He)(t))(XE)};function qb(t){return function(){return URL.createObjectURL(t)}}var QE=function(t){return function(r){return function(e){return Mt(Pb(t))(e)(function(){var n=Vn(Gn)(r);return function(a){return n(qb(a))}}())}}};var sf={ids:_(S)(jt(xp))(no),deleteFromCache:$d,disconnectXFromY:Md,connectXToY:wd,makeAllpass:Pd(Jt),makeAnalyser:Id(Jt),makeAudioWorkletNode:Rd(Jt),makeBandpass:Nd(Jt),makeConstant:Ld(Jt),makeConvolver:Bd(Jt),makeDelay:Hd(Jt),makeDynamicsCompressor:Ud(Jt),makeGain:Wd(Jt),makeHighpass:qd(Jt),makeHighshelf:Zd(Jt),makeIIRFilter:zd(Jt),makeLoopBuf:Vd(Jt),makeLowpass:Gd(Jt),makeLowshelf:Jd(Jt),makeMediaElement:jd(Jt),makeMicrophone:Xd(Jt),makeNotch:Qd(Jt),makePeaking:Kd(Jt),makePeriodicOsc:Yd(Jt),makePlayBuf:tb(Jt),makeRecorder:rb(Jt),makeSawtoothOsc:eb(Jt),makeSinOsc:nb(Jt),makeSpeaker:ab,makeSquareOsc:ob(Jt),makeStereoPanner:ub(Jt),makeTriangleOsc:ib(Jt),makeWaveShaper:cb(Jt),setAnalyserNodeCb:fb,setMediaRecorderCb:lb,setWaveShaperCurve:_b,setAudioWorkletParameter:pb,setBuffer:vb,setConvolverBuffer:Db,setDuration:Cb(Jt),setPeriodicOsc:db,setOnOff:wb,setBufferOffset:gb,setLoopStart:Ab,setLoopEnd:kb,setRatio:Tb,setOffset:Eb,setAttack:Sb,setGain:sb,setQ:mb,setPan:bb,setThreshold:yb,setRelease:hb,setKnee:xb,setDelay:Fb,setPlaybackRate:Ob,setFrequency:$b},Tt=function(t){return function(r){return P(Ue)(gm(Bb(r)))(function(){var e=Hb(t);return function(n){return gm(e(n))}}())}},Dp=function(t){var r=Ae(t);return function(e){return r(Wb(e))}};var va=function(t){return Ae(t)(Ub)},io=function(t){var r=Ae(t);return function(e){return r(Rb(e))}},Pn=function(t){return function(r){return Ae(t)(function(){var n=Dp(se)(r)();return zn(w)(n!=="closed")(Lb(r))()})}},QR=mt,KR=mt,Cm=function(t){return function(r){return _(Hi)(function(e){return{microphone:function(){return t?l(Wo)(QR(e)):W.value}(),camera:function(){return r?l(Wo)(KR(e)):W.value}()}})(gm(Ib(t)(r)))}};var _i=function(){function t(){}return t.value=new t,t}(),pi=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Hu=function(){function t(){}return t.value=new t,t}(),bn=Sd,Wi=function(t){return Ro(Wn)(I(qE)(Un(Wn)(P(Ue)(HE(t))(Ae(He))))(Un(Wn)(UE(ai("We navigated away from the page"))(t))))},kl=function(t){return function(r){return function(e){return function(n){return I(t)(l(r)(Hu.value))(n)}}}},Za=function(t){return function(r){return function(e){return function(n){return I(t)(l(r)(tt(ye)(De.value)(ne(T(n)))))(_(t.Functor0())(function(a){return tt(ye)(De.value)(ne(T(Q(ot)(a)(n))))})(_(t.Functor0())(function(a){return a.value0})(e)))}}}},hm=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return _(g)(function(i){return tt(ye)(De.value)(ne(T(function(){if(i.value0 instanceof _i)return l(w)(void 0);if(i.value0 instanceof pi)return Q(ot)(Q(ot)(i.value0.value0)(t(l(w)(void 0))))(r(Hu.value));if(i.value0 instanceof Hu)return function(){i.value1(),r(_i.value)();var p=No(P(Ue)(va(He))(function(s){return P(Ue)(io(He)(s))(function(f){return P(Ue)(e(s))(function(m){return Ae(He)(function(){var c=n(s)(m)(),h=Q(ot)(Q(ot)(c)(f))(Pn(se)(s));return r(new pi(h))(),h})})})}))();return t(function(){return r(Hu.value)(),li(Wi(p))()})(),void 0};throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 59, column 21 - line 77, column 26): "+[i.value0.constructor.name])}())))})(Ln(Vt(V))(I(R(D))(l(C(V))(l(w)(void 0)))(_(g)(function(i){return i.value0})(a)))(_(g)(et.create)(u)))}}}}}},za=function(t){return function(r){return function(e){return function(){return t(e)(),r(new ZE(e))()}}}},Em=function(t){return function(r){return function(e){return function(n){return function(a){return nn(function(u){return function(i){var o=kl(R(D))(C(V))(r)(i);return zf(I(R(D))(l(C(V))(tt(bs)(Xt.value)("cursor: pointer;")))(hm(e)(u)(n)(a)(r)(o)))([mn(_(g)(function(p){if(p instanceof Hu)return t;if(p instanceof _i)return"\u23F3";if(p instanceof pi)return"\u{1F6D1}";throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 127, column 17 - line 130, column 35): "+[p.constructor.name])})(o))])}})}}}}},wt=function(t){return function(r){return function(e){return function(n){return nn(function(a){return function(u){var i=kl(R(D))(C(V))(t)(u);return Mn(hm(r)(a)(e)(n)(t)(i))([mn(_(g)(function(o){if(o instanceof Hu)return"Turn on";if(o instanceof _i)return"Loading...";if(o instanceof pi)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 101, column 17 - line 104, column 42): "+[o.constructor.name])})(i))])}})}}}};var gl=function(t){return function(r){return function(){var n=gc(t)(),a=$t(ff([new Fi(_(g)(function(u){return Uf.create(xk(u))})(r))])(sf))(function(u){return u(n)})();return a}}};var yt=function(t){return function(r){return function(){var n=gc(t)(),a=$t(ff(r)(sf))(function(u){return u(n)})();return a}}},Tm=function(t){return function(){var e=va(se)();return _(S)(function(n){return Q(ot)(n)(Pn(se)(e))})(yt(e)(t))()}};var YR=function(){return d.value}(),KE=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}}))(d.value)(YR)({allpass:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([Wa(sr(Er)(a)(vt()))(function(u){return function(i){return Nt(lt)(.2)([u,ap(gd)(700)([ap(_m(Et(ht()(J(J(Ct)(fd)()()()({reflectSymbol:function(){return"q"}}))(fm)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:990,q:20})([u]),ap(gd)(1110)([u,ap(_m(Et(ht()(J(J(Ct)(fd)()()()({reflectSymbol:function(){return"q"}}))(fm)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:2010,q:30})([u])])])])}})])}}))})}}};function qi(t){return function(e,n,a){if(n===null)return new t(e);var u=e.byteLength,i=t.BYTES_PER_ELEMENT,o=Math.min(u,n>>>0);if(a===null)return new t(e,o);var p=Math.min((u-o)/i,a);return new t(e,o,p)}}var rN=qi(Uint8ClampedArray),eN=qi(Uint32Array),nN=qi(Uint16Array),YE=qi(Uint8Array),aN=qi(Int32Array),uN=qi(Int16Array),oN=qi(Int8Array),iN=qi(Float32Array),cN=qi(Float64Array);function t0(t){for(var r=t.length,e=new Array(r),n=0;n<r;n++)e[n]=t[n];return e}var Sm={create:YE,BinaryValue0:function(){}};var xm=function(t){return function(r){return function(){return t0(r)}}};var Cl=Pu,hl=Pu,El=Pu,du=Pu,bu=Pu,yu=Pu,Au=Pu,ku=Pu;function Fm(t){return t|0}var mN=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}},Cc=Ut(function(t){return function(){var e=Mi(),n=ie(!0)(),a=mN("fx","FRP.Event.Animate",function(){return cr(S)(Mt(Ls)(e)(function(){var o=Ee(n)();return zn(w)(o)(function(){return t(void 0)(),a(19)()})()}))}),u=a(15);return u(),kn(!1)(n)}});var vN="background-color: rgb(150,30,10);",DN="background-color: rgb(130,60,10);",dN="background-color: rgb(80,90,10);",bN="background-color: rgb(10,130,10);",yN="background-color: rgb(10,100,0);",AN=js(mu)(function(t){return Mr(wr(Gs)()(Ya)()(q_))(vN)(Mr(wr(ma)()(Sn)()(Ya))(DN)(Mr(wr(Iu)()(xn)()(Sn))(dN)(Mr(wr(Ru)()(Fn)()(xn))(bN)(Mr(wr(Nu)()(Lu)()(Fn))(yN)(ao)))))}),kN=function(t){return function(r){return function(e){return function(n){return Dm(vm(Et(ht()(J(J(Ct)(mm)()()()({reflectSymbol:function(){return"fftSize"}}))(t)()()()({reflectSymbol:function(){return"cb"}})))(gt()())))({cb:n,fftSize:nm.value})([sr(r)(e)(vt())])}}}},gN=function(){return d.value}(),Nr="background-color: rgb(255,255,255,0.0);",Hr=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return function(o){return function(p){return function(s){return function(f){return _(t)(function(m){var v=Z_(r)()(Z_(n)()(m)(p))(s);return v?tt(u)(Xt.value)(Z_(r)()(Z_(n)()(AN)(p))(s)):tt(u)(Xt.value)(Nr)})(f)}}}}}}}}}}},CN=function(){return 15/40}(),hN=function(){return 10/40}(),EN=function(){return 7/40}(),TN=function(){return 3/40}(),SN=function(){return 1/40}(),a0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="analyser">Analyser</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode">analyser node</a> provides methods to recuperate the analysed data of an input. This is how, for example, Google Meet shows the little animation around a microphone icon. Ocarina provides the possibility to use the analyser as the terminus of an audio graph <i>or</i> as part of a longer DSP chain, as in the following example. The example uses an FFT size of 256, which is indicated in Ocarina as <code>TTT8</code> (two to the eighth power).</p>

  <pre><code>analyser_ { cb, fftSize: TTT8 } [ loopBuf atar bangOn ]</code></pre>

  ~analyser~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}}))(gN)({analyser:B(nn(function(n){return function(a){var u=f_(Ou(D))(rt(nt))(a),i=kl(R(D))(C(V))(e)(function(p){return p.right}(u)),o=function(p){return p.left}(u);return zr([Mn(I(R(D))(l(C(V))(tt(qf)(Xt.value)("cursor: pointer;")))(hm(t)(function(p){return n(Yt.create(p))})(function(p){return Tt(p)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(p){return function(s){return function(){var m=ie(W.value)(),v=gc(p)(),c=ff([kN(sm)(Er)(s)(function(ut){return function(){return kn(new F(ut))(m)(),kn(W.value)(m)}})])(sf),h=$t(I(R(w))(_(g)(Yt.create)(c))(_(g)(Kt.create)(Cc)))(function(ut){if(ut instanceof Yt)return ut.value0(v);if(ut instanceof Kt)return function(){var fr=Ee(m)();return jn(w)(te)(fr)(function(Zt){return function(){var re=mp(Zt)(),pt=xm(Sm)(re)(),mr=ie(0)(),Fr=ie(0)(),ir=ie(0)(),je=ie(0)(),Uo=ie(0)(),co=ie(0)(),Xi=ie(0)(),hu=ie(0)(),yn=ie(0)(),vi=ie(0)(),vf=function(Pe){if(Pe<32)return mr;if(Pe<64)return Fr;if(Pe<96)return ir;if(Pe<128)return je;if(Pe<168)return Uo;if(Pe<160)return co;if(Pe<224)return Xi;if(oe)return hu;throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 144, column 45 - line 152, column 63): "+[Pe.constructor.name])};Vl(pt)(function(Pe){var Di=Fm(Pe);return function(){var di=Ee(vi)();return Lc(Br(qu)(Di))(yn)(),Lc(Br(qu)(Di))(vf(di))(),Lc(Br(qu)(1))(vi)()}})();var Df=Nn(PC)(w)(function(Pe){return function(){var df=_(S)(jr)(Ee(Pe))(),di=_(S)(vo(zl)(df))(_(S)(jr)(Ee(yn)))();return Mr(wr(Gs)()(Ya)()(q_))(di>CN)(Mr(wr(ma)()(Sn)()(Ya))(di>hN)(Mr(wr(Iu)()(xn)()(Sn))(di>EN)(Mr(wr(Ru)()(Fn)()(xn))(di>TN)(Mr(wr(Nu)()(Lu)()(Fn))(di>SN)(ao)))))}})(Mr(wr(OC)()(BD)()(MC))(mr)(Mr(wr($C)()(HD)()(BD))(Fr)(Mr(wr(wC)()(q_)()(HD))(ir)(Mr(wr(Gs)()(Ya)()(q_))(je)(Mr(wr(ma)()(Sn)()(Ya))(Uo)(Mr(wr(Iu)()(xn)()(Sn))(co)(Mr(wr(Ru)()(Fn)()(xn))(Xi)(Mr(wr(Nu)()(Lu)()(Fn))(hu)(ao)))))))))();return n(new Kt(Df))()}})()};throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 125, column 57 - line 163, column 57): "+[ut.constructor.name])})();return function(){return h(),function(){var fr=Dp(se)(p)();return zn(w)(fr!=="closed")(Pn(se)(p))()}(),n(new Kt(js(mu)(T(js(Ua)(T(!1))))))()}}}})(e)(i)))([mn(_(g)(function(p){if(p instanceof Hu)return"Turn on";if(p instanceof _i)return"Loading...";if(p instanceof pi)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 176, column 31 - line 179, column 56): "+[p.constructor.name])})(i))]),hr(l(C(V))(tt(Dt)(Xt.value)("display: grid; grid-template-columns: repeat(8, 1fr); grid-auto-rows: 20px;")))([hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Me)(Sa)(Me)(Po)(Dt)(Sa)(Po)(ku)(ku)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Me)(Sa)(ra)(Mo)(Dt)(Sa)(Mo)(Au)(ku)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Me)(Sa)(ta)(wo)(Dt)(Sa)(wo)(yu)(ku)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Me)(Sa)(Yn)($o)(Dt)(Sa)($o)(bu)(ku)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Me)(Sa)(Kn)(Oo)(Dt)(Sa)(Oo)(du)(ku)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Me)(Sa)(Ua)(Fo)(Dt)(Sa)(Fo)(El)(ku)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Me)(Sa)(oi)(xo)(Dt)(Sa)(xo)(hl)(ku)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Me)(Sa)(ui)(So)(Dt)(Sa)(So)(Cl)(ku)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ra)(Ta)(Me)(Po)(Dt)(Ta)(Po)(ku)(Au)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ra)(Ta)(ra)(Mo)(Dt)(Ta)(Mo)(Au)(Au)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ra)(Ta)(ta)(wo)(Dt)(Ta)(wo)(yu)(Au)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ra)(Ta)(Yn)($o)(Dt)(Ta)($o)(bu)(Au)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ra)(Ta)(Kn)(Oo)(Dt)(Ta)(Oo)(du)(Au)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ra)(Ta)(Ua)(Fo)(Dt)(Ta)(Fo)(El)(Au)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ra)(Ta)(oi)(xo)(Dt)(Ta)(xo)(hl)(Au)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ra)(Ta)(ui)(So)(Dt)(Ta)(So)(Cl)(Au)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ta)(Ea)(Me)(Po)(Dt)(Ea)(Po)(ku)(yu)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ta)(Ea)(ra)(Mo)(Dt)(Ea)(Mo)(Au)(yu)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ta)(Ea)(ta)(wo)(Dt)(Ea)(wo)(yu)(yu)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ta)(Ea)(Yn)($o)(Dt)(Ea)($o)(bu)(yu)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ta)(Ea)(Kn)(Oo)(Dt)(Ea)(Oo)(du)(yu)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ta)(Ea)(Ua)(Fo)(Dt)(Ea)(Fo)(El)(yu)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ta)(Ea)(oi)(xo)(Dt)(Ea)(xo)(hl)(yu)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ta)(Ea)(ui)(So)(Dt)(Ea)(So)(Cl)(yu)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Yn)(ha)(Me)(Po)(Dt)(ha)(Po)(ku)(bu)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Yn)(ha)(ra)(Mo)(Dt)(ha)(Mo)(Au)(bu)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Yn)(ha)(ta)(wo)(Dt)(ha)(wo)(yu)(bu)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Yn)(ha)(Yn)($o)(Dt)(ha)($o)(bu)(bu)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Yn)(ha)(Kn)(Oo)(Dt)(ha)(Oo)(du)(bu)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Yn)(ha)(Ua)(Fo)(Dt)(ha)(Fo)(El)(bu)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Yn)(ha)(oi)(xo)(Dt)(ha)(xo)(hl)(bu)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Yn)(ha)(ui)(So)(Dt)(ha)(So)(Cl)(bu)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Kn)(Ca)(Me)(Po)(Dt)(Ca)(Po)(ku)(du)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Kn)(Ca)(ra)(Mo)(Dt)(Ca)(Mo)(Au)(du)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Kn)(Ca)(ta)(wo)(Dt)(Ca)(wo)(yu)(du)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Kn)(Ca)(Yn)($o)(Dt)(Ca)($o)(bu)(du)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Kn)(Ca)(Kn)(Oo)(Dt)(Ca)(Oo)(du)(du)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Kn)(Ca)(Ua)(Fo)(Dt)(Ca)(Fo)(El)(du)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Kn)(Ca)(oi)(xo)(Dt)(Ca)(xo)(hl)(du)(o)))([]),hr(I(R(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Kn)(Ca)(ui)(So)(Dt)(Ca)(So)(Cl)(du)(o)))([])])])}}))})}}};var FN=function(){return d.value}(),u0=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}}))(d.value)(FN)({bandpass:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([Wa(sr(Er)(a)(vt()))(function(u){return function(i){return Nt(lt)(.8)([wn(dn(Et(ht()(J(J(Ct)($n)()()()({reflectSymbol:function(){return"q"}}))(Dn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:400,q:1})([u]),wn(dn(Et(ht()(J(J(Ct)($n)()()()({reflectSymbol:function(){return"q"}}))(Dn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:880,q:5})([u]),wn(dn(Et(ht()(J(J(Ct)($n)()()()({reflectSymbol:function(){return"q"}}))(Dn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:1200,q:10})([u]),wn(dn(Et(ht()(J(J(Ct)($n)()()()({reflectSymbol:function(){return"q"}}))(Dn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:2e3,q:20})([u]),wn(dn(Et(ht()(J(J(Ct)($n)()()()({reflectSymbol:function(){return"q"}}))(Dn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:3e3,q:30})([u])])}})])}}))})}}};var $N=function(){return d.value}(),o0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(H()(Y)({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}}))($N)({compression:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([dE(aE(Et(ht()(Ct))(gt()())))({})([sr(Er)(a)(vt())])])}}))})}}};var Da=function(){return function(t){var r=_n(),e=le()({reflectSymbol:function(){return"playbackRate"}})(d.value),n=uf(t);return function(a){return r(e(n(a)))}}},mf=function(){return function(t){var r=_n(),e=le()({reflectSymbol:function(){return"onOff"}})(d.value),n=sh(t);return function(a){return r(e(n(a)))}}},i0=function(){return function(t){var r=_n(),e=le()({reflectSymbol:function(){return"offset"}})(d.value),n=uf(t);return function(a){return r(e(n(a)))}}},c0=function(){var t=_n(),r=le()({reflectSymbol:function(){return"loopStart"}})(d.value);return function(e){return t(r(e))}},f0=function(){var t=_n(),r=le()({reflectSymbol:function(){return"loopEnd"}})(d.value);return function(e){return t(r(e))}},In=function(){return function(t){var r=_n(),e=le()({reflectSymbol:function(){return"gain"}})(d.value),n=uf(t);return function(a){return r(e(n(a)))}}},Lo=function(){return function(t){var r=_n(),e=le()({reflectSymbol:function(){return"frequency"}})(d.value),n=uf(t);return function(a){return r(e(n(a)))}}};var Tl=function(){return function(t){var r=_n(),e=le()({reflectSymbol:function(){return"delayTime"}})(d.value),n=uf(t);return function(a){return r(e(n(a)))}}};var MN=function(){return d.value}(),l0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="constant">Constant value</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode">Constant values</a>, or DC offset, is a way to output an unchanging stream of values. This is only really useful when testing the performance of speakers or microphones and/or when working with a custom audio node that supports constant streaming values. Note that the constant source node in the web audio API can <i>also</i> be used to control audio parameters. Ocarina uses this feature of constant nodes under the hood to optimize certain computations.</p>

  <p>The following example abuses a constant audio node by turning it into a gnarly inpulse generator. We'll learn about the tie fighter symbol <code>~tf~</code> and the <code>pure</code> in the next section on Events. Kids, don't try this at home!</p>

  <pre><code>~txt~</code></pre>

  ~constant~
  </section>
`}})()()(H()(H()(H()(Y)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"tf"}})({reflectSymbol:function(){return"tf"}}))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}}))(MN)({tf:B(pe("<|>")),txt:B(pe(`run2_
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
  ]`)),constant:B(wt(e)(t)(function(n){return l(xa)(void 0)})(function(n){return function(a){return yt(n)([Nt(lt)(.5)([bm(cm)(0)(I(R(w))(vt())(l(C(G))(i0()(Hn)({d:5,o:.1,p:yo(ki)(function(u){return T(function(){var i=ou(mo)(u)(3)===0;return i?1:0}())})(An(0)(1920))}))))])])}}))})}}};var IN=function(){return d.value}(),_0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="convolution">Convolution</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConvolverNode">Convolution</a>, aka reverb, is a way to graft the shape of one sound (usually an <a href="https://en.wikipedia.org/wiki/Impulse_response">impulse response</a>) onto another. Convolution can sound great, but it is a <i>very expensive operation</i> that will cause noticeable artifacts on low-end devices. When shipping audio code to production, you're usually better off using an Audio Worklet Node with reverb optimized for your specific case. That said, for PoCs or hobbyist projects, convolution is great!</p>

  <pre><code>\\{loop, verb} -> run2_
  [ convolver verb [ loopBuf loop bangOn ] ]</code></pre>

  ~convolution~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}}))(IN)({convolution:B(wt(e)(t)(function(n){return Wt(LE)(_(Hi)(function(a){return function(u){return{loop:a,verb:u}}})(Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")))(Tt(n)("https://cdn.jsdelivr.net/gh/andibrae/Reverb.js/Library/StMarysAbbeyReconstructionPhase3.m4a"))})(function(n){return function(a){return yt(n)([_E(Sh)(a.verb)([sr(Er)(a.loop)(vt())])])}}))})}}};var NN=function(){return d.value}(),p0=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}}))(d.value)(NN)({delay:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return yt(n)([Wa(aa(tu)(a)(vt()))(function(u){return function(i){return Nt(lt)(.2)([Io(un)(.03)([u]),Io(un)(.1)([u]),Io(un)(.3)([u]),Io(un)(.7)([u])])}})])}}))})}}};var BN=function(){return d.value}(),s0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(H()(Y)({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}}))(BN)({gain:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return yt(n)([Nt(lt)(.1)([sr(Er)(a)(vt())])])}}))})}}};var UN=function(){return d.value}(),m0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="highpass">Highpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highpass filter</a> lets higher frequencies pass and amortizes lower ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ highpass_ 2000.0
      [ loopBuf buf bangOn ]
  ]
</code></pre>

  ~highpass~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}}))(UN)({highpass:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([sl(vu)(2e3)([sr(Er)(a)(vt())])])}}))})}}};var qN=function(){return d.value}(),v0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="highshelf">Highshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highshelf filter</a> boosts or attenuates high frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
  [ highshelf_ { frequency: 2000.0, gain: 0. }
      [ loopBuf buf bangOn ]
  ]</code></pre>

  ~highshelf~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}}))(qN)({highshelf:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([bE(nE(Et(ht()(J(J(Ct)(Ph)()()()({reflectSymbol:function(){return"gain"}}))(Ih)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:2e3,gain:.4})([sr(Er)(a)(vt())])])}}))})}}};var zN=function(){return d.value}(),D0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(H()(Y)({reflectType:function(){return"iirFilterEx"}})({reflectSymbol:function(){return"iirFilterEx"}}))(zN)({iirFilterEx:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([pE()()(Th(Pi)(Pi))(new et(Jc()()(20298e-8)(Jc()()(.0004059599)(Jc()()(20298e-8)(Jv))),Jc()()(1.0126964558)(Jc()()(-1.9991880801)(Jc()()(.9873035442)(Jv)))))([sr(Er)(a)(vt())])])}}))})}}};var GN=function(){return d.value}(),d0=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}}))(d.value)(GN)({loopBuf:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/100/100981_1234256-lq.mp3")})(function(n){return function(a){return yt(n)([sr(cf(Et(ht()(J(J(J(J(Ct)(ll)()()()({reflectSymbol:function(){return"playbackRate"}}))(Y_)()()()({reflectSymbol:function(){return"loopStart"}}))(K_)()()()({reflectSymbol:function(){return"loopEnd"}}))(of)()()()({reflectSymbol:function(){return"buffer"}})))(gt()())))({buffer:a,playbackRate:.5,loopStart:.1,loopEnd:.6})(vt()),sr(cf(Et(ht()(J(J(J(J(Ct)(ll)()()()({reflectSymbol:function(){return"playbackRate"}}))(Y_)()()()({reflectSymbol:function(){return"loopStart"}}))(K_)()()()({reflectSymbol:function(){return"loopEnd"}}))(of)()()()({reflectSymbol:function(){return"buffer"}})))(gt()())))({buffer:a,playbackRate:1,loopStart:.5,loopEnd:1.2})(vt()),sr(cf(Et(ht()(J(J(Ct)(ll)()()()({reflectSymbol:function(){return"playbackRate"}}))(of)()()()({reflectSymbol:function(){return"buffer"}})))(gt()())))({buffer:a,playbackRate:1.7})(vt())])}}))})}}};var jN=function(){return d.value}(),b0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="lowpass">Lowpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowpass filter</a> lets lower frequencies pass and amortizes higher ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ lowpass_ 215.0 [ loopBuf buf bangOn ] ]
</code></pre>

  ~lowpass~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}}))(jN)({lowpass:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([ml(lm)(215)([sr(Er)(a)(vt())])])}}))})}}};var QN=function(){return d.value}(),y0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="lowshelf">Lowshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowshelf filter</a> boosts or attenuates lower frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
   [ lowshelf_ { frequency: 91.0, gain: 10.0 }
       [ loopBuf buf bangOn ]
   ]
</code></pre>

  ~lowshelf~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}}))(QN)({lowshelf:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([AE(eE(Et(ht()(J(J(Ct)($h)()()()({reflectSymbol:function(){return"gain"}}))(wh)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:91,gain:.4})([sr(Er)(a)(vt())])])}}))})}}};var YN=function(){return d.value}(),A0=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}}))(d.value)(YN)({microphone:B(wt(e)(t)(function(n){return Cm(!0)(!1)})(function(n){return function(a){return yt(n)([function(){if(a.microphone instanceof F)return Du(function(u){return Nt(lt)(1)([ep(X_)(a.microphone.value0),Io(un)(.1)([Nt(lt)(.2)([u])])])});if(a.microphone instanceof W)return Nt(lt)(.02)([vE(kc)(440)]);throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Microphone (line 45, column 15 - line 50, column 56): "+[a.microphone.constructor.name])}()])}}))})}}};var rL=function(){return d.value}(),k0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(H()(Y)({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}}))(rL)({notch:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([vl(pl(Et(ht()(J(J(Ct)(cl)()()()({reflectSymbol:function(){return"q"}}))(fl)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:400,q:1})(l(de)(vl(pl(Et(ht()(J(J(Ct)(cl)()()()({reflectSymbol:function(){return"q"}}))(fl)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:880,q:5})(l(de)(vl(pl(Et(ht()(J(J(Ct)(cl)()()()({reflectSymbol:function(){return"q"}}))(fl)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:1200,q:10})(l(de)(vl(pl(Et(ht()(J(J(Ct)(cl)()()()({reflectSymbol:function(){return"q"}}))(fl)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:2e3,q:20})(l(de)(vl(pl(Et(ht()(J(J(Ct)(cl)()()()({reflectSymbol:function(){return"q"}}))(fl)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:3e3,q:30})(l(de)(sr(Er)(a)(vt())))))))))))])}}))})}}};var nL=function(){return d.value}(),g0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(H()(Y)({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}}))(nL)({peaking:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([Dl(_l(Et(ht()(J(J(J(Ct)(ul)()()()({reflectSymbol:function(){return"q"}}))(ol)()()()({reflectSymbol:function(){return"gain"}}))(il)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:400,q:1,gain:-20})(l(de)(Dl(_l(Et(ht()(J(J(J(Ct)(ul)()()()({reflectSymbol:function(){return"q"}}))(ol)()()()({reflectSymbol:function(){return"gain"}}))(il)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:880,q:5,gain:20})(l(de)(Dl(_l(Et(ht()(J(J(J(Ct)(ul)()()()({reflectSymbol:function(){return"q"}}))(ol)()()()({reflectSymbol:function(){return"gain"}}))(il)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:1200,q:10,gain:-20})(l(de)(Dl(_l(Et(ht()(J(J(J(Ct)(ul)()()()({reflectSymbol:function(){return"q"}}))(ol)()()()({reflectSymbol:function(){return"gain"}}))(il)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:2e3,q:20,gain:20})(l(de)(Dl(_l(Et(ht()(J(J(J(Ct)(ul)()()()({reflectSymbol:function(){return"q"}}))(ol)()()()({reflectSymbol:function(){return"gain"}}))(il)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:3e3,q:30,gain:-20})(l(de)(sr(Er)(a)(vt())))))))))))])}}))})}}};var uL=function(){return d.value}(),C0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(H()(Y)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(uL)({periodic:B(wt(e)(t)(function(n){return l(xa)(void 0)})(function(n){return function(a){return yt(n)([Nt(lt)(.2)([Bi(Li(Et(ht()(J(J(Ct)(Ni(Ii(ma)))()()()({reflectSymbol:function(){return"spec"}}))(Ri)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:140,spec:new et(Mr(wr(ma)()(Sn)()(Ya))(.1)(Mr(wr(Iu)()(xn)()(Sn))(.2)(Mr(wr(Ru)()(Fn)()(xn))(.3)(Mr(wr(Nu)()(Lu)()(Fn))(.4)(ao)))),Mr(wr(ma)()(Sn)()(Ya))(.4)(Mr(wr(Iu)()(xn)()(Sn))(.3)(Mr(wr(Ru)()(Fn)()(xn))(.2)(Mr(wr(Nu)()(Lu)()(Fn))(.1)(ao)))))})(vt())])])}}))})}}};var iL=function(){return d.value}(),h0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(H()(Y)({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}}))(iL)({playBuf:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/470/470035_9564355-lq.mp3")})(function(n){return function(a){return yt(n)([aa(tp(Et(ht()(J(J(J(Ct)(Fh)()()()({reflectSymbol:function(){return"duration"}}))(xh)()()()({reflectSymbol:function(){return"bufferOffset"}}))(Q_)()()()({reflectSymbol:function(){return"buffer"}})))(gt()())))({buffer:a,duration:3,bufferOffset:4.2})(vt())])}}))})}}};var Zb=function(){function t(){}return t.value=new t,t}();var E0={attr:function(t){return function(r){return b({key:"controls",value:q(r)})}}};var zb=function(){function t(){}return t.value=new t,t}();var T0={attr:function(t){return function(r){return b({key:"src",value:q(r)})}}};var Vb=function(t){return function(r){return new M(X("audio")(t)(U(r)))}};var pL=function(t){return function(r){return function(e){return function(n){return hd(t)(n)(ep(r)(e))}}}},sL=function(){return d.value}(),S0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="recorder">Recorder</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioDestinationNode">recorder</a> allows you to record audio. It takes a callback that you can use to stash the recorded audio somewhere, like in a file for example, as the example below does. You can use it as a simple note-taking app \u{1F399}\uFE0F.</p>

  <pre><code>\\cb m -> recorder cb (microphone m)</code></pre>

  ~recorder~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}}))(sL)({recorder:B(nn(function(n){return function(a){var u=f_(Ou(D))(rt(nt))(a),i=f_(Ou(D))(rt(nt))(function(f){return f.left}(u)),o=function(f){return f.right}(i),p=kl(R(D))(C(V))(e)(function(f){return f.right}(u)),s=function(f){return f.left}(i);return zr([Mn(I(R(D))(l(C(V))(tt(qf)(Xt.value)("cursor: pointer;")))(_(g)(function(f){return tt(ye)(De.value)(ne(T(function(){if(f.e instanceof _i)return l(w)(void 0);if(f.e instanceof pi)return Q(ot)(Q(ot)(Q(ot)(f.e.value0)(t(l(w)(void 0))))(jn(w)(te)(f.rec)(function(){var m=P_(gD);return function(v){return m(km(v))}}())))(n(Yt.create(Hu.value)));if(f.e instanceof Hu)return function(){f.cncl();var v=rp();n(new Yt(_i.value))();var c=No(P(Ue)(_(Hi)(function(h){return h.microphone})(Cm(!0)(!1)))(function(h){return Ae(He)(function(){var dt=Jt(l(w)(l(w)(void 0)))(function(fr){return function(){var Kr=va(se)(),re=gc(Kr)(),pt=ff([pL(ud)(X_)(fr)(function(Fr){return function(){return n(new Kt(new Yt(Fr)))(),cr(S)(pm(Fr)(v))(),QE("audio/ogg; codecs=opus")(function(je){return n(Kt.create(Kt.create(je)))})(Fr)()}})])(sf),mr=$t(pt)(function(Fr){return Fr(re)})();return function(){mr(),P(Gn)(fE(v))(ce(w)(te)(function(){var je=P_(gD);return function(Uo){return je(km(Uo))}}()))();var ir=Dp(se)(Kr)();return zn(w)(ir!=="closed")(Pn(se)(Kr))()}}})(h)();return n(new Yt(new pi(dt)))(),dt})}))();return t(function(){return n(Yt.create(Hu.value))(),li(Wi(c))()})(),void 0};throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Recorder (line 65, column 47 - line 108, column 52): "+[f.e.constructor.name])}())))})(Ln(Vt(V))(I(R(D))(l(C(V))(W.value))(_(g)(F.create)(o)))(_(g)(bf)(Ln(Vt(V))(I(R(D))(l(C(V))(l(w)(void 0)))(_(g)(function(f){return f.value0})(e)))(_(g)(function(f){return function(m){return function(v){return{e:f,cncl:m,rec:v}}}})(p)))))))([mn(_(g)(function(f){if(f instanceof Hu)return"Turn on";if(f instanceof _i)return"Loading...";if(f instanceof pi)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Recorder (line 119, column 31 - line 122, column 56): "+[f.constructor.name])})(p))]),zr([Vb(I(R(D))(l(C(V))(tt(E0)(Zb.value)("true")))(I(R(D))(l(C(V))(tt(cD)(Xt.value)("display:none;")))(I(R(D))(_(g)(function(f){return tt(T0)(zb.value)(f)})(s))(_(g)(T(tt(cD)(Xt.value)("display:block;")))(s)))))([])])])}}))})}}};var vL=function(){return d.value}(),x0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sawtoothOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(vL)({periodic:B(wt(e)(t)(function(n){return l(xa)(void 0)})(function(n){return function(a){return yt(n)([Nt(lt)(.2)([mE(Eh)(448)(vt())])])}}))})}}};var dL=function(){return d.value}(),F0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="sine">Sine wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sine wave oscillator</a> plays back a sine wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sinOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(dL)({periodic:B(wt(e)(t)(function(n){return l(xa)(void 0)})(function(n){return function(a){return yt(n)([Nt(lt)(.2)([lf(kc)(448)(vt())])])}}))})}}};var yL=function(){return d.value}(),O0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="sawtooth">Square wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ squareOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(yL)({periodic:B(wt(e)(t)(function(n){return l(xa)(void 0)})(function(n){return function(a){return yt(n)([Nt(lt)(.2)([np(al)(448)(vt())])])}}))})}}};var kL=function(){return d.value}(),$0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/StereoPannerNode">stereo panner</a> pans audio in the stereo plane. <code>-1.0</code> represents hard left, and <code>1.0</code> represents hard right, as in the example below.</p>

  <pre><code>\\buf -> run2_
  [ pan_ 1.0 [ loopBuf buf bangOn ] ]</code></pre>

  ~pan~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}}))(kL)({pan:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return yt(n)([kE(hh)(1)([sr(Er)(a)(vt())])])}}))})}}};var CL=function(){return d.value}(),w0=It({reflectType:function(){return`<ul>
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
`}})()()(Y)(CL)({});var EL=function(){return d.value}(),M0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ triangleOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(EL)({periodic:B(wt(e)(t)(function(n){return l(xa)(void 0)})(function(n){return function(a){return yt(n)([Nt(lt)(.2)([dm(im)(448)(vt())])])}}))})}}};var SL=function(){return d.value}(),P0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="waveshaper">Waveshaper</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/WaveshaperNode">waveshaper node</a>, aka distortion, uses a <a href="https://en.wikipedia.org/wiki/Waveshaper">waveshaping function</a> to add warmth to a sound.</p>

  <pre><code>~code~</code></pre>

  ~waveShaper~
  </section>
`}})()()(H()(H()(Y)({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}}))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}}))(SL)({code:B(pe(`do
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
    [ waveShaper (makeFloatArray (makeDistortionCurve 400.0)) [ loopBuf buf bangOn ] ]`)),waveShaper:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){var u=function(i){var o=dc/180;return _($r)(function(p){var s=jr(p)*2/jr(44100)-1;return(3+i)*s*20*o/(dc+i*Ym(Ja)(Mc)(s))})(An(0)(44099))};return yt(n)([gE(rE)(Mb(u(400)))([sr(Er)(a)(vt())])])}}))})}}};var FL=function(){return d.value}(),I0=function(t){return function(r){return function(e){return function(n){var a=Q(ot)(r(pf.value))(bn),u=za(t)(e);return It({reflectType:function(){return`<div>
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
</div>`}})()()(H()(H()(H()(H()(H()(H()(H()(H()(H()(H()(H()(H()(H()(H()(H()(H()(vn()(H()(H()(H()(H()(H()(H()(H()(H()(H()(H()(H()(H()(Y)({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}}))({reflectType:function(){return"triangleOsc"}})({reflectSymbol:function(){return"triangleOsc"}}))({reflectType:function(){return"toc"}})({reflectSymbol:function(){return"toc"}}))({reflectType:function(){return"squareOsc"}})({reflectSymbol:function(){return"squareOsc"}}))({reflectType:function(){return"sinOsc"}})({reflectSymbol:function(){return"sinOsc"}}))({reflectType:function(){return"sawtoothOsc"}})({reflectSymbol:function(){return"sawtoothOsc"}}))({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}}))({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}}))({reflectType:function(){return"periodicOsc"}})({reflectSymbol:function(){return"periodicOsc"}}))({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}}))({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}}))({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}}))({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}}))({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}}))({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}}))({reflectType:function(){return"iirFilter"}})({reflectSymbol:function(){return"iirFilter"}}))({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}}))({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}}))({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}}))({reflectType:function(){return"drumroll"}})({reflectSymbol:function(){return"drumroll"}}))({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}}))({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}}))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}}))({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}}))({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}}))({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}}))({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}}))(FL)({drumroll:B(Em("\u{1F941}")(n)(u)(function(i){return Tt(i)("https://freesound.org/data/previews/50/50711_179538-lq.mp3")})(function(i){return function(o){return yt(i)([Nt(lt)(1)([sr(Er)(o)(vt())])])}})),toc:B(w0),allpass:B(KE(u)(r)(n)),analyser:B(a0(u)(r)(n)),bandpass:B(u0(u)(r)(n)),constant:B(l0(u)(r)(n)),compression:B(o0(u)(r)(n)),convolution:B(_0(u)(r)(n)),delay:B(p0(u)(r)(n)),gain:B(s0(u)(r)(n)),highpass:B(m0(u)(r)(n)),highshelf:B(v0(u)(r)(n)),iirFilter:B(D0(u)(r)(n)),loopBuf:B(d0(u)(r)(n)),lowshelf:B(y0(u)(r)(n)),lowpass:B(b0(u)(r)(n)),notch:B(k0(u)(r)(n)),playBuf:B(h0(u)(r)(n)),peaking:B(g0(u)(r)(n)),microphone:B(A0(u)(r)(n)),pan:B($0(u)(r)(n)),periodicOsc:B(C0(u)(r)(n)),recorder:B(S0(u)(r)(n)),sawtoothOsc:B(x0(u)(r)(n)),sinOsc:B(F0(u)(r)(n)),squareOsc:B(O0(u)(r)(n)),triangleOsc:B(M0(u)(r)(n)),waveShaper:B(P0(u)(r)(n)),next:Za(R(D))(C(V))(n)(a)})}}}};var Gb=function(){function t(){}return t.value=new t,t}(),R0={attr:function(t){return function(r){return b({key:"checked",value:q(r)})}}};var Bo=function(){function t(){}return t.value=new t,t}();var si={attr:function(t){return function(r){return b({key:"type",value:q(r)})}}};var Ho=function(t){return function(r){return new M(X("input")(t)(U(r)))}};var ML=function(t){return t},$m=function(t){return function(r){return function(e){return Si(t)(I(t.Alternative0().Plus1().Alt0())(l(t.Alternative0().Applicative0())(r))(e))}}};var yp=function(t){return function(r){return t(r)}},hc=function(t){return{map:function(r){return function(e){return function(n){return e(_(t)(function(a){return function(u){return a(r(u))}})(n))}}}}},Zi=function(t){return function(r){return function(e){return function(n){return yp(_(hc(t))(r)(e))(_(t)(Yi)(n))}}}};var Sl=function(t){return Zi(t)(T)};var gu=ML;var N0=function(t){return function(r){return function(e){return gu(function(n){return Ce(t)(I(t.Alternative0().Plus1().Alt0())(l(t.Alternative0().Applicative0())(yp(r)(n)))(_(t.Filterable1().Functor1())(function(a){return yp(a)(n)})(e)))})}}},Jb=function(t){return{apply:function(r){return function(e){return function(n){return e(r(_(t)(Wu(bi))(n)))}}},Functor0:function(){return hc(t)}}};var xl=function(t){return function(r){return Ut(function(e){return $t(r)(function(n){return function(){var u=vp(t)();return e({acTime:u,value:n})()}})})}};var L0=function(t){return function(r){return function(e){var n=function(a){return function(u){return function(i){return function(o){return function(p){return function(s){return function(){var m=Ee(i)();return zn(w)(m)(function(){var c=vp(t)(),h=es(wg(Zu(Ja)(u-c-.04)(.01)*1e3))(function(){var dt=Ee(i)();return zn(w)(dt)(function(){return kn(u)(p)(),a(u)(),n(a)(u+s)(i)(o)(p)(s)()})()})();return kn(new F(h))(o)()})()}}}}}}};return Ut(function(a){return function(){var i=ie(!0)(),o=ie(W.value)(),p=vp(t)(),s=ie(p+r)();n(a)(r)(i)(o)(s)(r)();var f=$t(e)(function(m){return function(){P(Gn)(Ee(o))(ce(w)(te)(m_))();var c=Ee(s)();return n(a)(c+m)(i)(o)(s)(m)()}})();return Q(ot)(Q(ot)(f)(kn(!1)(i)))(P(Gn)(Ee(o))(ce(w)(te)(m_)))}})}}};var Va=function(t){return function(r){return function(e){return function(n){return function(a){var u=e===t||n===r;if(u)return r;var i=(n-r)/(e-t),o=r-i*t;return i*a+o}}}}};var PL=function(){return d.value}(),B0=function(t){return function(r){return function(e){return function(n){return It({reflectType:function(){return`<section>
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

</section>`}})()()(H()(H()(Y)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}}))(PL)({txt:B(pe(`module Main where

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
import FRP.Event.Class (fold, mapAccum, sampleOn)
import FRP.Event.VBus (V, vbus)
import Data.Number (pi, sin)
import Type.Proxy (Proxy(..))
import Ocarina.Clock (withACTime)
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
                        pure (D.Xtype := "checkbox")
                        pure (D.OnClick := cb (const (e unit)))
                        startE $> (D.Checked := "false")
                    )
                    []
                )
                ([ _.cbx0, _.cbx1, _.cbx2, _.cbx3 ] <@> push.cbx)
            )
        ]
  )`)),empl:B(eo()()(Wr({reflectSymbol:function(){return"cbx"}})(qr(ro(su()(Wr({reflectSymbol:function(){return"cbx0"}})(qr(_e))(Wr({reflectSymbol:function(){return"cbx1"}})(qr(_e))(Wr({reflectSymbol:function(){return"cbx2"}})(qr(_e))(Wr({reflectSymbol:function(){return"cbx3"}})(qr(_e))(Qn)()())()())()())()()))))(Wr({reflectSymbol:function(){return"startStop"}})(qr(ro(su()(Wr({reflectSymbol:function(){return"start"}})(qr(_e))(Wr({reflectSymbol:function(){return"stop"}})(qr(_e))(Qn)()())()()))))(Qn)()())()())(Qu({reflectSymbol:function(){return"cbx"}})()()()(fe({reflectSymbol:function(){return"cbx0"}})()()(fe({reflectSymbol:function(){return"cbx1"}})()()(fe({reflectSymbol:function(){return"cbx2"}})()()(fe({reflectSymbol:function(){return"cbx3"}})()()(Xn)()())()())()())()())(Qu({reflectSymbol:function(){return"startStop"}})()()()(fe({reflectSymbol:function(){return"start"}})()()(fe({reflectSymbol:function(){return"stop"}})()()(Xn)()())()())(Xn)()())()())(d.value)(function(a){return function(u){var i=I(R(D))(l(C(V))(void 0))(u.startStop.start),o=function(v){return $m(Vt(G))(!1)(ju(Vt(G))(T(lu(Xa)))(pu(v))(!1))},p=o(u.cbx.cbx3),s=o(u.cbx.cbx2),f=o(u.cbx.cbx1),m=o(u.cbx.cbx0);return zr([Mn(Cn(Bt)(E(D))(_(g)(function(){var v=tt(ye)(De.value);return function(c){return v(ne(T(c)))}}()))([Gr(g)(Ln(Vt(V))(I(R(D))(l(C(V))(l(w)(void 0)))(_(g)(function(v){return v.value0})(n)))(K(g)(i)(rt(nt))))(function(v){return function(){v();var h=va(se)(),ut=io(se)(h)(),dt=function(Kr){return function(re){return function(pt){return v_(Vt(G))(function(mr){return function(Fr){var ir=Fr.value1+(mr.value1-Fr.value0)*function(){return mr.value0?Kr:1}();return new et(new et(mr.value1,ir),ir)}})(Zi(g)(et.create)(re)(pt))(new et(0,0))}}},fr=gl(h)(Xo(G)(_(g)(function(){var Kr=Br($a)(.04);return function(re){return Kr(function(pt){return pt.acTime}(re))}}())(xl(h)(Cc)))(function(Kr){var re=function(je){return function(Uo){return Si(Vt(G))(Kr)(_(g)(bf)(Si(Vt(G))(Uo)(_(g)(function(co){return function(Xi){return function(hu){return{f:co,a:Xi,t:hu}}}})(je))))}},pt=_(g)(function(je){return je?4:1})(Sl(g)(p)(Kr)),mr=dt(4)(s)(Kr),Fr=_(g)(function(je){return je?4:1})(Sl(g)(f)(Kr)),ir=dt(8)(m)(Kr);return[Je(lt)(0)(Gr(g)(re(ir)(Fr))(function(je){return In()(qe)({n:Va(1)(.01)(4)(.15)(je.a)*ks(dc*je.f)+.15,o:je.t,t:ii})}))([Bi(Li(Et(ht()(J(J(Ct)(Ni(Ii(ma)))()()()({reflectSymbol:function(){return"spec"}}))(Ri)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:325.6,spec:new et(Mr(wr(ma)()(Sn)()(Ya))(.3)(Mr(wr(Iu)()(xn)()(Sn))(-.1)(Mr(wr(Ru)()(Fn)()(xn))(.7)(Mr(wr(Nu)()(Lu)()(Fn))(-.4)(ao)))),Mr(wr(ma)()(Sn)()(Ya))(.6)(Mr(wr(Iu)()(xn)()(Sn))(.3)(Mr(wr(Ru)()(Fn)()(xn))(.2)(Mr(wr(Nu)()(Lu)()(Fn))(0)(ao)))))})(xe(Bt)(E(w))([vt(),Gr(g)(re(mr)(pt))(function(je){return Lo()(qe)({n:325.6+Va(1)(3)(4)(15.5)(je.a)*ks(dc*je.f),o:je.t,t:ii})})]))])]}))(),Zt=Q(ot)(Q(ot)(fr)(ut))(Pn(se)(h));return t(Q(ot)(Zt)(a.startStop.start(void 0)))(),a.startStop.stop(Zt)()}}),Gr(g)(u.startStop.stop)(function(v){return Q(ot)(v)(Q(ot)(t(l(w)(void 0)))(a.startStop.start(void 0)))})]))([mn(xe(Bt)(E(D))([K(g)(i)("Turn on"),K(g)(u.startStop.stop)("Turn off")]))]),hr(Cn(Bt)(E(D))(_(g)(tt(Dt)(Xt.value)))([K(g)(u.startStop.stop)("display:block;"),K(g)(i)("display:none;")]))(_($r)(function(v){return Ho(xe(Bt)(E(D))([l(C(V))(tt(si)(Bo.value)("checkbox")),l(C(V))(tt(ye)(De.value)(ne(T(v(void 0))))),K(g)(i)(tt(R0)(Gb.value)("false"))]))([])})(zm($r)([function(v){return v.cbx0},function(v){return v.cbx1},function(v){return v.cbx2},function(v){return v.cbx3}])(a.cbx)))])}}))})}}}};var jb={recip:function(t){return 1/t},Ring0:function(){return Mc}};var Xb=function(t){return function(r){return{EuclideanRing0:function(){return t},DivisionRing1:function(){return r}}}};function Fl(t){return function(){return function(r){return t(r)()}}}function Ol(t){return function(r){return function(e){return function(n){return function(){return n.addEventListener(t,r,e)}}}}}function $l(t){return function(r){return function(e){return function(n){return function(){return n.removeEventListener(t,r,e)}}}}}function Qb(t){return t.clientX}function Kb(t){return t.clientY}function Ap(t){return t.button}var kp=Rt("MouseEvent");var H0=function(t){return function(r){return Ut(function(e){return $t(r)(function(n){return function(){var u=Ee(t.buttons)();return e({value:n,buttons:u})()}})})}};var U0=function(){var r=ie(W.value)(),e=ie(Lv)(),n=_(S)(PD)(Mi)(),a=Fl(function(p){return ce(w)(te)(function(s){return kn(new F({x:Qb(s),y:Kb(s)}))(r)})(kp(p))})(),u=Fl(function(p){return ce(w)(te)(function(s){return Sf(mk(tn)(Ap(s)))(e)})(kp(p))})(),i=Fl(function(p){return ce(w)(te)(function(s){return Sf(rs(tn)(Ap(s)))(e)})(kp(p))})();Ol(_n()("mousemove"))(a)(!1)(n)(),Ol(_n()("mousedown"))(u)(!1)(n)(),Ol(_n()("mouseup"))(i)(!1)(n)();var o=function(){return $l(_n()("mousemove"))(a)(!1)(n)(),$l(_n()("mousedown"))(u)(!1)(n)(),$l(_n()("mouseup"))(i)(!1)(n)()};return{position:r,buttons:e,dispose:o}},W0=Ut(function(t){return function(){var e=_(S)(PD)(Mi)(),n=Fl(function(a){return ce(w)(te)(function(u){return t(Ap(u))})(kp(a))})();return Ol(_n()("mousedown"))(n)(!1)(e)(),$l(_n()("mousedown"))(n)(!1)(e)}});var Z0=function(t){return gu(function(r){return _(g)(function(e){return e.value(e.buttons)})(H0(t)(r))})};var ry=function(t){return t};function Pm(){return Date.now()}var vT=function(t){return Ut(function(r){return $t(t)(function(e){return function(){var a=Pm();return r({time:a,value:e})()}})})};var D1=gu(function(t){return _(g)(function(r){return r.value(r.time)})(vT(t))}),ny=_(hc(g))(function(){var t=ip(RE);return function(r){return t(ry(r))}}())(D1);var b1=function(t){var r=function(u){return function(i){return function(o){return function(p){return function(s){return function(f){return function(m){var v=Br(i.DivisionRing1().Ring0().Semiring0())(ya(i.DivisionRing1().Ring0().Semiring0()))(ya(i.DivisionRing1().Ring0().Semiring0())),c=function(h){return function(ut){if(h.last instanceof W)return ut;if(h.last instanceof F)return Br(o)(ut)(p(function(dt){return vo(i.EuclideanRing0())(Rn(i.DivisionRing1().Ring0().Semiring0())(dt(Br(o)(h.last.value0.value1)(h.now.value1)))(Tu(i.DivisionRing1().Ring0())(h.now.value0)(h.last.value0.value0)))(v)}));throw new Error("Failed pattern match at Ocarina.Example.Docs.FixEx (line 103, column 5 - line 103, column 35): "+[h.constructor.name,ut.constructor.name])}};return gu(function(h){var ut=yp(m)(K(u.Filterable1().Functor1())(h)(rt(nt))),dt=as(u)(Zi(u.Filterable1().Functor1())(et.create)(f)(ut)),fr=ju(u)(c)(dt)(s);return Si(u)(fr)(h)})}}}}}}},e=function(u){return function(i){return r(u)(i)(i.DivisionRing1().Ring0().Semiring0())(function(o){return o(rt(nt))})}},n=function(u){return function(i){return gu(function(o){return D_(Vt(G))(function(p){var s=i($m(Vt(G))(u)(p));return{input:Sl(g)(s)(o),output:Si(Vt(G))(p)(o)}})})}},a=function(u){return function(i){return function(o){if(sk(u))return-8*(i-1)-o*2;if(oe)return 2*(4-i);throw new Error("Failed pattern match at Ocarina.Example.Docs.FixEx (line 63, column 3 - line 65, column 34): "+[u.constructor.name,i.constructor.name,o.constructor.name])}}};return n(2)(function(u){return e(Vt(G))(Xb(zl)(jb))(2)(_(hc(g))(Te())(ny))(function(){var i=n(10)(function(o){return e(Vt(G))(Xb(zl)(jb))(10)(_(hc(g))(Te())(ny))(Wt(Jb(g))(Wt(Jb(g))(_(hc(g))(a)(Z0(t)))(u))(o))});return N0(Vt(G))(i)(K(g)(W0)(i))}())})},y1=function(){return d.value}(),DT=function(t){return function(r){return function(e){return function(n){return It({reflectType:function(){return`<section>
  <h2>Fix</h2>

  <p>Fix, like it's equivalent in ocarina that we've already seen, creates a feedback loop. However, in this case, we are talking about a feedback loop of <i>events</i>, not sound.</p>

  <p>At first glance, it may not be clear why we need an event stream to feed back into itself? It seems prone to saturation: if you have a counter that feeds back into itself with a delay, after a few seconds you'll have so many events that it will crash your browser (I've tried it!).</p>

  <p>However, there's one important circumstance where you need fixed points: when an event can only be defined in terms of itself. One classic category of this is the <i>differential equation</i>. Differential equations allow you to produce <a href="https://en.wikipedia.org/wiki/Simple_harmonic_motion">Slinky effects, aka simple harmonic motion,</a> and a lot of other neat behaviors that are difficult to produce via other means.</p>

  <p>Let's listen to the sound of simple harmonic motion in the example below, courtesy of <code>fix</code>. The differential equation in the example below comes from Phil Freeman, the creator of the PureScript language and the author of the <code>purescript-behaviors</code> package. When you click "Turn on", you won't hear much, but press and release your mouse anywhere on the screen to hear the differential equation take flight!</p>

  <pre><code>~txt~</code></pre>

  ~empl~

  <p>When working with stateful events, a good way to decide if you should use <code>fold</code> versus <code>fix</code> is to ask the following question: can I incrementally change my state based on an initial state, or is my state defined in terms of how it changes? If you can incrementally change your state, go with <code>fold</code>. If, on the other hand, your state is defined in terms of how it changes, go with <code>fix</code>.</p>
</section>`}})()()(H()(H()(Y)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}}))(y1)({txt:B(pe(`module Main

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
import FRP.Event.Class (class IsEvent, fix, fold, sampleOn, withLast)
import FRP.Event.Mouse (Mouse, down, getMouse)
import FRP.Event.VBus (V, vbus)
import Test.QuickCheck (arbitrary, mkSeed)
import Test.QuickCheck.Gen (evalGen)
import Type.Proxy (Proxy(..))
import Ocarina.Clock (withACTime)
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
  )`)),empl:B(eo()()(Wr({reflectSymbol:function(){return"start"}})(qr(_e))(Wr({reflectSymbol:function(){return"stop"}})(qr(_e))(Qn)()())()())(fe({reflectSymbol:function(){return"start"}})()()(fe({reflectSymbol:function(){return"stop"}})()()(Xn)()())()())(d.value)(function(a){return function(u){var i=I(R(D))(l(C(V))(void 0))(u.start);return zr([Mn(Cn(Bt)(E(D))(_(g)(function(){var o=tt(ye)(De.value);return function(p){return o(ne(T(p)))}}()))([Gr(g)(Ln(Vt(V))(I(R(D))(l(C(V))(l(w)(void 0)))(_(g)(function(o){return o.value0})(n)))(K(g)(i)(rt(nt))))(function(o){return function(){o();var s=va(se)(),f=io(se)(s)(),m=U0(),v=F_(0)(1e4)(),c=function(pt){return{o:pt.value0+.04,n:pt.value1,t:ii}},h=_(To)(function(pt){return pt-.5})(U_(iC)),ut=P(Yc)(h)(function(pt){return P(Yc)(h)(function(mr){return P(Yc)(h)(function(Fr){return P(Yc)(h)(function(ir){return l(B_)(Mr(wr(ma)()(Sn)()(Ya))(pt)(Mr(wr(Iu)()(xn)()(Sn))(mr)(Mr(wr(Ru)()(Fn)()(xn))(Fr)(Mr(wr(Nu)()(Lu)()(Fn))(ir)(ao)))))})})})}),dt=Wt(tf)(_(To)(et.create)(ut))(ut),fr=Wt(tf)(Wt(tf)(Wt(tf)(_(To)(function(pt){return function(mr){return function(Fr){return function(ir){return{s0:pt,s1:mr,s2:Fr,s3:ir}}}}})(dt))(dt))(dt))(dt),Zt=Kf(fr)({newSeed:Jf(v),size:5}),Kr=gl(s)(Xo(G)(_(g)(function(pt){return new et(pt.acTime,pt.value)})(xl(s)(Sl(g)(b1(m))(Cc))))(function(pt){return[Je(lt)(0)(_(g)(function(){var mr=In()(qe),Fr=ea(On)(function(ir){return Zu(Ja)(-.4)(.5*(ir-1))});return function(ir){return mr(c(Fr(ir)))}}())(pt))([ml(bd(Et(ht()(J(J(Ct)(Mh)()()()({reflectSymbol:function(){return"q"}}))(od)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:90.4,q:20})([DE(al)(90.4)])]),Je(lt)(0)(_(g)(function(){var mr=In()(qe),Fr=ea(On)(function(ir){return Zu(Ja)(-.2)(.4*(ir-3))});return function(ir){return mr(c(Fr(ir)))}}())(pt))([wn(dn(Et(ht()(J(J(Ct)($n)()()()({reflectSymbol:function(){return"q"}}))(Dn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:90.4*4,q:20})([Bi(Li(Et(ht()(J(J(Ct)(Ni(Ii(ma)))()()()({reflectSymbol:function(){return"spec"}}))(Ri)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:90.4*3.02,spec:Zt.s0})(I(R(w))(vt())(_(g)(function(){var mr=Lo()(qe),Fr=ea(On)(function(ir){return 90.4*3.02+14*(ir-1)});return function(ir){return mr(c(Fr(ir)))}}())(pt)))])]),Je(lt)(0)(_(g)(function(){var mr=In()(qe),Fr=ea(On)(function(ir){return Zu(Ja)(-.1)(.2*(ir-6))});return function(ir){return mr(c(Fr(ir)))}}())(pt))([wn(dn(Et(ht()(J(J(Ct)($n)()()()({reflectSymbol:function(){return"q"}}))(Dn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:90.4*6,q:20})([Bi(Li(Et(ht()(J(J(Ct)(Ni(Ii(ma)))()()()({reflectSymbol:function(){return"spec"}}))(Ri)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:90.4*5.07,spec:Zt.s1})(I(R(w))(vt())(_(g)(function(){var mr=Lo()(qe),Fr=ea(On)(function(ir){return 90.4*5.07+18*(ir-1)});return function(ir){return mr(c(Fr(ir)))}}())(pt)))])]),Je(lt)(0)(_(g)(function(){var mr=In()(qe),Fr=ea(On)(function(ir){return Zu(Ja)(0)(.2*(ir-3))});return function(ir){return mr(c(Fr(ir)))}}())(pt))([wn(dn(Et(ht()(J(J(Ct)($n)()()()({reflectSymbol:function(){return"q"}}))(Dn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:90.4*8,q:20})([Bi(Li(Et(ht()(J(J(Ct)(Ni(Ii(ma)))()()()({reflectSymbol:function(){return"spec"}}))(Ri)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:90.4*7.13,spec:Zt.s2})(I(R(w))(vt())(_(g)(function(){var mr=Lo()(qe),Fr=ea(On)(function(ir){return 90.4*7.13+32*(ir-1)});return function(ir){return mr(c(Fr(ir)))}}())(pt)))])]),Je(lt)(0)(_(g)(function(){var mr=In()(qe),Fr=ea(On)(function(ir){return Zu(Ja)(0)(.1*(ir-7))});return function(ir){return mr(c(Fr(ir)))}}())(pt))([Bi(Li(Et(ht()(J(J(Ct)(Ni(Ii(ma)))()()()({reflectSymbol:function(){return"spec"}}))(Ri)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:90.4*9.14,spec:Zt.s3})(I(R(w))(vt())(_(g)(function(){var mr=Lo()(qe),Fr=ea(On)(function(ir){return 90.4*9.14+31*(ir-1)});return function(ir){return mr(c(Fr(ir)))}}())(pt)))])]}))(),re=Q(ot)(Q(ot)(Kr)(f))(Pn(se)(s));return t(Q(ot)(re)(a.start(void 0)))(),a.stop(re)()}}),Gr(g)(u.stop)(function(o){return Q(ot)(o)(Q(ot)(t(l(w)(void 0)))(a.start(void 0)))})]))([mn(xe(Bt)(E(D))([K(g)(i)("Turn on"),K(g)(u.stop)("Turn off")]))])])}}))})}}}};var k1=function(){return d.value}(),dT=function(t){return function(r){return function(e){return function(n){var a=za(t)(e);return It({reflectType:function(){return`<div>
  <h1>State</h1>

  <h3>Or Events 2.0</h3>
  <p>
    The name of this section is a bit of a nisnomer. While it will address the issue of maintaining state in an audio graph, it's really just about two mechanisms you can use to make an <code>Event</code> stateful. One is called <code>fold</code>, and the other is called <code>fix</code>. Both are part of the <code>IsEvent</code> typeclass, which means you get them for free when working with events.
  </p>

  ~fold~
  ~fix~

  <h2>Next steps</h2>
  <p>Using <code>fold</code> and <code>fix</code>, we can create internal state in our Web Audio works that would be really tedious and error-prone to achieve in vanilla JS or other compile-to-JS languages. There's still one nagging issue that we haven't addressed, though. For all of the flexibility we can achieve with events, we still can't flex the audio graph itself, meaning that we can't add or remove components. In the next section, we'll learn how to do that with <a ~next~ style="cursor:pointer;">subgraphs</a>.</p>
</div>`}})()()(H()(H()(vn()(Y)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"fold"}})({reflectSymbol:function(){return"fold"}}))({reflectType:function(){return"fix"}})({reflectSymbol:function(){return"fix"}}))(k1)({next:Za(R(D))(C(V))(n)(Q(ot)(r(sp.value))(bn)),fold:B(B0(a)(r)(e)(n)),fix:B(DT(a)(r)(e)(n))})}}}};var C1=function(){function t(){}return t.value=new t,t}(),bT=function(){function t(){}return t.value=new t,t}(),ay=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),h1=`module Main where

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
`;var E1=function(){return d.value}(),T1=function(t){return function(r){return function(e){return l(t)(mf(r)(af)({x:ad,o:e}))}}},S1=function(t){return function(r){return function(e){return l(t)(mf(r)(af)({x:kh,o:e}))}}},x1=ka(en)(jr)(function(t){var r=function(a){return I(R(w))(T1(C(G))()(a+.27*(t*Dc(1.005)(t))))(S1(C(G))()(a+3+.3*(t*Dc(1.005)(t))))},e=function(a){return l(C(G))(In()(Hn)({p:[0,.4,.1,.05,.01,0],o:a+.3*(t*Dc(1.005)(t)),d:.8}))},n=function(a){return function(u){return Je(lt)(0)(e(a))([lf(kc)(200+t*u)(r(a))])}};return[n(.2)(4),n(.3)(6),n(.45)(14),n(.7)(20)]}),yT=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(H()(H()(Y)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}}))(d.value)(E1)({txt:B(pe(h1)),ex0:B(nn(function(n){return ka(en)(function(a){return I(R(D))(l(C(V))(C1.value))(a)})(function(a){return zr([Mn(Gr(g)(Ln(Vt(V))(I(R(D))(l(C(V))(l(w)(void 0)))(_(g)(function(u){return u.value0})(e)))(_(g)(et.create)(a)))(function(u){return tt(ye)(De.value)(ne(T(function(){return u.value0 instanceof ay?Q(ot)(Q(ot)(u.value0.value0)(n(bT.value)))(t(l(w)(void 0))):function(){u.value1();var o=Tm([Nt(lt)(1)(nu(yi)(_($r)(x1)(An(0)(100))))])();return t(Q(ot)(o)(n(bT.value)))(),n(new ay(o))()}}())))}))([mn(Gr(g)(a)(function(u){return u instanceof ay?"Turn off":"Turn on"}))])])})}))})}}};var zi=function(){function t(){}return t.value=new t,t}();var Tc={attr:function(t){return function(r){return b({key:"max",value:q(r)})}}};var Vi=function(){function t(){}return t.value=new t,t}();var Sc={attr:function(t){return function(r){return b({key:"min",value:q(r)})}}};var Gi=function(){function t(){}return t.value=new t,t}();var xc={attr:function(t){return function(r){return b({key:"input",value:st(r)})}}};var Ji=function(){function t(){}return t.value=new t,t}(),Fc={attr:function(t){return function(r){return b({key:"step",value:q(r)})}}};var ji=function(){function t(){}return t.value=new t,t}();var Oc={attr:function(t){return function(r){return b({key:"value",value:q(r)})}}};var mi=function(t){return function(r){return function(e){return I(t)(r)(e(void 0))}}};var O1=Ug,Cu={convert:function(t){return t}},gp={convert:function(t){return w_(t)}},kT=function(t){return t},uy=function(t){return t.convert},eu=function(t){return function(r){return function(e){return bt(O1)(w_(r))(uy(t)(e(void 0)))}}};var Cp=function(t){return function(r){return function(e){return function(n){return Cn(Wg)(r)(e)(kT(uy(t)(n)))}}}};function CT(t){return t.target}var wl=function(t){return an(CT(t))};var M1=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (switcher, text, text_)
import Deku.Core (Domable)
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
    :: forall lock payload
     . Maybe BrowserAudioBuffer
    -> Domable Effect lock payload
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
`,P1=function(){return d.value}(),I1="https://freesound.org/data/previews/100/100981_1234256-lq.mp3",hT=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(H()(H()(H()(Y)({reflectType:function(){return"wagtxt"}})({reflectSymbol:function(){return"wagtxt"}}))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))(d.value)(P1)({wagtxt:B(pe(`run2_
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
          (add <$> (pure 0.0 <|> sl1))`)),txt:B(pe(M1)),ex1:B(eo()()(Wr({reflectSymbol:function(){return"slider"}})(qr(ro(su()(Wr({reflectSymbol:function(){return"s0"}})(qr(_e))(Wr({reflectSymbol:function(){return"s1"}})(qr(_e))(Wr({reflectSymbol:function(){return"s2"}})(qr(_e))(Qn)()())()())()()))))(Wr({reflectSymbol:function(){return"startStop"}})(qr(ro(su()(Wr({reflectSymbol:function(){return"loading"}})(qr(_e))(Wr({reflectSymbol:function(){return"start"}})(qr(_e))(Wr({reflectSymbol:function(){return"stop"}})(qr(_e))(Qn)()())()())()()))))(Qn)()())()())(Qu({reflectSymbol:function(){return"slider"}})()()()(fe({reflectSymbol:function(){return"s0"}})()()(fe({reflectSymbol:function(){return"s1"}})()()(fe({reflectSymbol:function(){return"s2"}})()()(Xn)()())()())()())(Qu({reflectSymbol:function(){return"startStop"}})()()()(fe({reflectSymbol:function(){return"loading"}})()()(fe({reflectSymbol:function(){return"start"}})()()(fe({reflectSymbol:function(){return"stop"}})()()(Xn)()())()())()())(Xn)()())()())(d.value)(function(n){return function(a){var u=I(R(D))(a.startStop.start)(l(C(V))(void 0)),i=pu(a.slider.s2),o=pu(a.slider.s1),p=pu(a.slider.s0),s=function(f){return sr(cf(Et(ht()(J(J(J(J(Ct)(ll)()()()({reflectSymbol:function(){return"playbackRate"}}))(Y_)()()()({reflectSymbol:function(){return"loopStart"}}))(K_)()()()({reflectSymbol:function(){return"loopEnd"}}))(of)()()()({reflectSymbol:function(){return"buffer"}})))(gt()())))({buffer:f,playbackRate:2.6,loopStart:.6,loopEnd:1.1})(mi(R(w))(vt())(function(){return mi(R(w))(_(g)(function(){var m=Da()(om),v=Va(0)(.2)(100)(5);return function(c){return m(v(c))}}())(p))(function(){return mi(R(w))(_(g)(function(){var m=c0(),v=Va(0)(0)(100)(1.2);return function(c){return m(v(c))}}())(o))(function(){return _(g)(function(){var m=f0(),v=Va(0)(.05)(100)(1);return function(c){return m(v(c))}}())(Ln(Vt(G))(i)(_(g)(Br($a))(I(R(w))(l(C(G))(0))(o))))})})}))};return zr(bt(cn)(_($r)(function(f){return zr([pe(f.l),Ho(Cp(Cu)(E(D))(l(C(V)))(eu(Cu)(tt(si)(Bo.value)("range"))(function(){return eu(Cu)(tt(Sc)(Vi.value)("0"))(function(){return eu(Cu)(tt(Tc)(zi.value)("100"))(function(){return eu(Cu)(tt(Fc)(Ji.value)("1"))(function(){return eu(gp)(tt(Oc)(ji.value)("50"))(function(){return tt(xc)(Gi.value)(ne(function(){var m=ce(w)(te)(Af(Gn)(rf)(f.f)),v=Vn(wa)(Yf);return function(c){return m(v(wl(c)))}}()))})})})})})))([])])})([{l:"Playback rate",f:n.slider.s0},{l:"Loop start",f:n.slider.s1},{l:"Loop end",f:n.slider.s2}]))([Mn(Cp(Cu)(E(D))(_(g)(function(){var f=tt(ye)(De.value);return function(m){return f(ne(T(m)))}}()))(eu(Cu)(K(g)(a.startStop.loading)(l(w)(void 0)))(function(){return eu(gp)(Gr(g)(a.startStop.stop)(function(f){return Q(ot)(f)(Q(ot)(t(l(w)(void 0)))(n.startStop.start(void 0)))}))(function(){return Gr(g)(Ln(Vt(V))(I(R(D))(l(C(V))(l(w)(void 0)))(_(g)(function(f){return f.value0})(e)))(K(g)(u)(rt(nt))))(function(f){return function(){f(),n.startStop.loading(void 0)();var v=No(P(Ue)(va(He))(function(c){return P(Ue)(io(He)(c))(function(h){return P(Ue)(Tt(c)(I1))(function(ut){return Ae(He)(function(){var fr=yt(c)([s(ut)])(),Zt=Q(ot)(Q(ot)(fr)(h))(Pn(se)(c));return n.startStop.stop(Zt)(),Zt})})})}))();return t(function(){return n.startStop.start(void 0)(),li(Wi(v))()})(),void 0}})})})))([mn(mi(R(D))(_(g)(T("Turn off"))(a.startStop.stop))(function(){return _(g)(T("Turn on"))(u)}))])]))}}))})}}};var N1=`module Main where

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
import Ocarina.Clock (interval)
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
                        $ toEvent event.slider
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
  )`,L1=gu(function(t){return Ut(function(r){return $t(t)(function(e){return function(){var a=no();return r(e(a))()}})})}),B1=function(){return d.value}(),H1=function(t){if(t<.142857)return 261.625565;if(t<.285714)return 293.664768;if(t<.428571)return 349.228231;if(t<.571429)return 391.995436;if(t<.714286)return 440;if(t<.857143)return 523.251131;if(oe)return 587.329536;throw new Error("Failed pattern match at Ocarina.Example.Docs.Events.Ex2 (line 227, column 1 - line 227, column 23): "+[t.constructor.name])},ET=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<section>
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

  <p>One important optimization we make here is the use of the function <code>memoize</code>. Whenever we're dealing with audio-clock timing, we want to limit the number of subscriptions to receive events from the audio clock. Ideally, there is only one subscription that takes a reading of the clock as a single source of truth. Because we are in PureScript-land, events (like everything else), are referrentially transparent, meaning that new ones will get created every time you use them (just like a new <code>2</code> is created every time you type the value <code>2</code>: they don't all refer to one uber-<code>2</code>). To sync all the events to the <i>same</i> source, we use <code>memoize</code>. While this optimization is not necessary, I recommend it: it will make sure the timing is 100% accurate at a very small energy cost (meaning <code>memoize</code> will eat up slightly more power from a phone's battery, but still not much).</p>

  <pre><code>@txt@</code></pre>

  @ex2@

</section>
`}})({reflectType:function(){return"@"}})()()(H()(H()(Y)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}}))(d.value)(B1)({txt:B(pe(N1)),ex2:B(eo()()(Wr({reflectSymbol:function(){return"slider"}})(qr(_e))(Wr({reflectSymbol:function(){return"startStop"}})(qr(ro(su()(Wr({reflectSymbol:function(){return"start"}})(qr(_e))(Wr({reflectSymbol:function(){return"stop"}})(qr(_e))(Qn)()())()()))))(Qn)()())()())(fe({reflectSymbol:function(){return"slider"}})()()(Qu({reflectSymbol:function(){return"startStop"}})()()()(fe({reflectSymbol:function(){return"start"}})()()(fe({reflectSymbol:function(){return"stop"}})()()(Xn)()())()())(Xn)()())()())(d.value)(function(n){return function(a){var u=I(R(D))(a.startStop.start)(l(C(V))(void 0)),i=function(o){return Xo(G)(o)(function(p){var s=_(g)(function(){var ut=Br($a)(.01);return function(dt){return ut(ln(dt))}}())(p),f=_(g)(Qa)(p),m=I(R(w))(vt())(_(g)(function(){var ut=Lo()(om);return function(dt){return ut(H1(dt))}}())(f)),v=_(g)(function(ut){return am(function(dt){return{p:[0,.15,.05,.01,.005,5e-4,0],d:.4,o:dt}}(ut))})(s),c=_(g)(function(ut){return am(function(dt){return{p:[0,.3,.1,.05,.01,.005,0],d:.4,o:dt}}(ut))})(s),h=_(g)(function(ut){return am(function(dt){return{p:[0,.6,.2,.1,.5,.03,0],d:.4,o:dt}}(ut))})(s);return[Wa(dm(im)(0)(m))(function(ut){return function(dt){return Nt(lt)(2)([Je(lt)(0)(_(g)(In()(Hn))(h))([wn(dn(Et(ht()(J(J(Ct)($n)()()()({reflectSymbol:function(){return"q"}}))(Dn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:1e3,q:20})([ut])]),Je(lt)(0)(_(g)(In()(Hn))(c))([wn(dn(Et(ht()(J(J(Ct)($n)()()()({reflectSymbol:function(){return"q"}}))(Dn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:2e3,q:20})([ut])]),Je(lt)(0)(_(g)(In()(Hn))(v))([sl(yd(Et(ht()(J(J(Ct)(Rh)()()()({reflectSymbol:function(){return"q"}}))(id)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:4e3,q:20})([ut])])])}})]})};return zr([zr([pe("tempo"),Ho(Cp(Cu)(E(D))(l(C(V)))(eu(Cu)(tt(si)(Bo.value)("range"))(function(){return eu(Cu)(tt(Sc)(Vi.value)("0"))(function(){return eu(Cu)(tt(Tc)(zi.value)("100"))(function(){return eu(Cu)(tt(Fc)(Ji.value)("1"))(function(){return eu(gp)(tt(Oc)(ji.value)("50"))(function(){return tt(xc)(Gi.value)(ne(function(){var o=ce(w)(te)(Af(Gn)(rf)(n.slider)),p=Vn(wa)(Yf);return function(s){return o(p(wl(s)))}}()))})})})})})))([])]),Mn(Cn(Bt)(E(D))(_(g)(function(){var o=tt(ye)(De.value);return function(p){return o(ne(T(p)))}}()))([Gr(g)(Ln(Vt(V))(I(R(D))(l(C(V))(l(w)(void 0)))(_(g)(function(o){return o.value0})(e)))(K(g)(u)(rt(nt))))(function(o){return function(){o();var s=va(se)(),f=Zi(g)(et.create)(L1)(L0(s)(.91)(_(g)(Va(0)(.42)(100)(1.4))(pu(a.slider)))),m=gl(s)(i(f))(),v=Q(ot)(m)(Pn(se)(s));return t(Q(ot)(v)(n.startStop.start(void 0)))(),n.startStop.stop(Q(ot)(v)(Pn(se)(s)))()}}),Gr(g)(a.startStop.stop)(function(o){return Q(ot)(o)(Q(ot)(t(l(w)(void 0)))(n.startStop.start(void 0)))})]))([mn(xe(Bt)(E(D))([K(g)(u)("Turn on"),K(g)(a.startStop.stop)("Turn off")]))])])}}))})}}};var W1=function(){return d.value}(),TT=function(){return Dr({reflectType:function(){return`<section>
  <h2>Three flavors of events.</h2>

  <p>When we're in the browser, events tend to come in three broad categories:</p>

  <ul>
    <li>Things that need to happen <span style="font-weight: 800;">now</span>.</li>
    <li>Things that happen as the result of a user interaction.</li>
    <li>Things that are scheduled to happen in the future, for example with <code>setTimeout</code>.</li>
  </ul>

  <p>The next three examples cover all three cases.</p>

</section>`}})({reflectType:function(){return"@"}})()()(Y)(d.value)(W1)({})}();var Z1=function(){return d.value}(),ST=function(){return Dr({reflectType:function(){return`<section>
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
</section>`}})({reflectType:function(){return"@"}})()()(Y)(d.value)(Z1)({})}();var V1=function(){return d.value}(),xT=function(){return Dr({reflectType:function(){return`<section>

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
    When using Ocarina, you have to get your events from somewhere. At a minimum, you'll consume a browser interaction like a click or swipe that turns on the audio. In fact, without some form of human interaction, most browsers will block the Web Audio API from turning on.
  </p>
  <p>
    <code>Events</code> are often produced within a web framework like <a href="https://github.com/mikesol/purescript-deku">Deku</a>, Halogen or React. They don't have to be, though - you can create and consume your own events.
  </p>

  <h3>Behavior</h3>

  <p>
    The <code>Behavior</code> type takes an event that needs to be "unlocked" (meaning in the form of <code>a -> b</code>, so an <code>a</code> is needed to unlock a <code>b</code>) and unlocks it with an <code>a</code>. Behaviors don't need to produce their <code>a</code> immediately. In fact, they don't need to produce it at all: it's entirely possible to create <code>Behavior (const empty)</code> that "mutes" the event. This resembles the physical world: when we want to observe a behavior, like the weather outside or the axial rotation of the Earth, there is a time-cost to observing anything that ranges from instantaneous to infinite.
  </p>

  <p>
    In Ocarina, we usually want to observe the behavior of things like a mouse's position, an audio buffer's content or a random number generator.
  </p>
</section>`}})({reflectType:function(){return"@"}})()()(Y)(d.value)(V1)({})}();var J1=function(){return d.value}(),FT=function(t){return function(r){return function(e){return function(n){var a=function(i){return Za(R(D))(C(V))(n)(Q(ot)(r(i))(bn))},u=za(t)(e);return Dr({reflectType:function(){return`<div>
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
</div>`}})({reflectType:function(){return"@"}})()()(H()(H()(H()(H()(H()(vn()(H()(Y)({reflectType:function(){return"primer"}})({reflectSymbol:function(){return"primer"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"inOcarina"}})({reflectSymbol:function(){return"inOcarina"}}))({reflectType:function(){return"flavors"}})({reflectSymbol:function(){return"flavors"}}))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}}))(d.value)(J1)({next:a(_p.value),primer:B(xT),inOcarina:B(ST),flavors:B(TT),ex0:B(yT(u)(r)(n)),ex1:B(hT(u)(r)(n)),ex2:B(ET(u)(r)(n))})}}}};var X1=function(){return d.value}(),OT=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(d.value)(X1)({ai0:B(wt(e)(t)(function(n){return Ro(Wn)(Wt(Ui)(Wt(Ui)(Wt(Ui)(_(yl)(function(a){return function(u){return function(i){return function(o){return{tink0:a,tink1:u,tink2:i,tink3:o}}}}})(Un(Wn)(Tt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Un(Wn)(Tt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Un(Wn)(Tt(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(Un(Wn)(Tt(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(a){return yt(n)([Nt(lt)(1)(function(){var u=function(i){return l(C(G))(mf()(af)(um()(Br($a)(i))(j_)))};return[aa(tu)(a.tink0)(u(.1)),aa(tu)(a.tink1)(u(.2)),aa(tu)(a.tink2)(u(.9)),aa(tu)(a.tink3)(u(1.8))]}())])}}))})}}};var K1=function(){return d.value}(),$T=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(d.value)(K1)({ai0:B(wt(e)(t)(function(n){return Ro(Wn)(Wt(Ui)(Wt(Ui)(Wt(Ui)(_(yl)(function(a){return function(u){return function(i){return function(o){return{tink0:a,tink1:u,tink2:i,tink3:o}}}}})(Un(Wn)(Tt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Un(Wn)(Tt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Un(Wn)(Tt(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(Un(Wn)(Tt(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(a){return yt(n)([Nt(lt)(1)(function(){var u=function(o){return l(C(G))(mf()(af)(um()(Br($a)(o))(j_)))},i=function(o){var p=ou(mo)(o)(4);return p===0?a.tink0:p===1?a.tink1:p===2?a.tink2:a.tink3};return Gr($r)(An(0)(100))(function(o){var p=jr(o);return aa(tu)(i(o))(u(.3+.3*(p*Dc(1.005)(p))))})}())])}}))})}}};var tB=function(){return d.value}(),wT=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(d.value)(tB)({ai0:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([Wa(sr(Er)(a)(vt()))(function(u){return function(i){return Nt(lt)(.8)([wn(dn(Et(ht()(J(J(Ct)($n)()()()({reflectSymbol:function(){return"q"}}))(Dn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:400,q:1})([u]),wn(dn(Et(ht()(J(J(Ct)($n)()()()({reflectSymbol:function(){return"q"}}))(Dn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:880,q:5})([u]),wn(dn(Et(ht()(J(J(Ct)($n)()()()({reflectSymbol:function(){return"q"}}))(Dn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:1200,q:10})([u]),wn(dn(Et(ht()(J(J(Ct)($n)()()()({reflectSymbol:function(){return"q"}}))(Dn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:2e3,q:20})([u]),wn(dn(Et(ht()(J(J(Ct)($n)()()()({reflectSymbol:function(){return"q"}}))(Dn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:3e3,q:30})([u])])}})])}}))})}}};var eB=function(){return d.value}(),MT=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(d.value)(eB)({ai0:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([Wa(sr(Er)(a)(vt()))(function(u){return function(i){return Nt(lt)(.8)(Gr($r)(An(0)(40))(ka(en)(jr)(function(o){return wn(dn(Et(ht()(J(J(Ct)($n)()()()({reflectSymbol:function(){return"q"}}))(Dn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:200+o*150,q:30})([u])})))}})])}}))})}}};var aB=function(){return d.value}(),PT=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<div>
  <pre><code>\\buf -> run2_
  [ fix
      \\b -> gain_ 1.0
        [ playBuf buf bangOn
        , delay_ 0.1 [ gain_ 0.6 [ b ] ]
        ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(d.value)(aB)({ai0:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(a){return yt(n)([Du(function(u){return Nt(lt)(1)([aa(tu)(a)(vt()),Io(un)(.1)([Nt(lt)(.6)([u])])])})])}}))})}}};var oB=function(){return d.value}(),iB=function(t){return function(r){return l(t)(In(r)(Hn)({p:[1,1,0],o:0,d:10}))}},cB=function(t){return function(r){return l(t)(In(r)(Hn)({p:[1,1,0],o:0,d:8}))}},Ml=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return Io(t)(n)([Nt(r)(a)([sl(e)(u)(i)])])}}}}}}},IT=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<div>
  <pre><code>@txt@</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(H()(H()(Y)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(d.value)(oB)({txt:B(pe(`dgh d g h i =
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
  ]`)),ai0:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(a){return yt(n)([Wa(aa(tu)(a)(vt()))(function(u){return function(i){return Du(function(o){return Nt(lt)(1)([u,Ml(un)(lt)(vu)(.15)(.7)(1500)([Du(function(p){return Je(lt)(1)(iB(C(G))())([Ml(un)(lt)(vu)(.4)(.5)(2500)([o,p])])})]),Ml(un)(lt)(vu)(.29)(.85)(2e3)([Du(function(p){return Nt(lt)(1)([Ml(un)(lt)(vu)(.6)(.6)(3500)([o,Du(function(s){return Je(lt)(1)(cB(C(G))())([Ml(un)(lt)(vu)(.75)(.6)(4e3)([p,s]),Ml(un)(lt)(vu)(.75)(.55)(3e3)([u])])})])])})])])})}})])}}))})}}};var lB=function(){return d.value}(),RT=function(t){return function(r){return function(e){return function(n){var a=function(u){return Za(R(D))(C(V))(n)(Q(ot)(r(u))(bn))};return Dr({reflectType:function(){return`<section>
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
  </section>`}})({reflectType:function(){return"@"}})()()(vn()(Y)({reflectType:function(){return"hwLink"}})({reflectSymbol:function(){return"hwLink"}}))(d.value)(lB)({hwLink:a(_f.value)})}}}};var pB=function(){return d.value}(),NT=function(t){return function(r){return function(e){return function(n){var a=function(i){return Za(R(D))(C(V))(n)(Q(ot)(r(i))(bn))},u=za(t)(e);return Dr({reflectType:function(){return`<div>
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
</div>`}})({reflectType:function(){return"@"}})()()(H()(H()(H()(H()(H()(H()(H()(vn()(Y)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"intro"}})({reflectSymbol:function(){return"intro"}}))({reflectType:function(){return"code5"}})({reflectSymbol:function(){return"code5"}}))({reflectType:function(){return"code4"}})({reflectSymbol:function(){return"code4"}}))({reflectType:function(){return"code3"}})({reflectSymbol:function(){return"code3"}}))({reflectType:function(){return"code2"}})({reflectSymbol:function(){return"code2"}}))({reflectType:function(){return"code1"}})({reflectSymbol:function(){return"code1"}}))({reflectType:function(){return"code0"}})({reflectSymbol:function(){return"code0"}}))(d.value)(pB)({intro:B(RT(t)(r)(e)(n)),next:a(fp.value),code0:B(OT(u)(r)(n)),code1:B($T(u)(r)(n)),code2:B(wT(u)(r)(n)),code3:B(MT(u)(r)(n)),code4:B(PT(u)(r)(n)),code5:B(IT(u)(r)(n))})}}}};var LT=function(t){return function(r){return new M(X("code")(t)(U(r)))}},cy=LT(O(E(D)));var BT=function(t){return function(r){return new M(X("pre")(t)(U(r)))}},fy=BT(O(E(D)));var DB=function(){return d.value}(),HT=function(t){return function(r){return function(e){return function(n){var a=Q(ot)(r(lp.value))(bn),u=za(t)(e);return Dr({reflectType:function(){return`<div>
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
</div>`}})({reflectType:function(){return"@"}})()()(H()(vn()(H()(Y)({reflectType:function(){return"result"}})({reflectSymbol:function(){return"result"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}}))(d.value)(DB)({code:B(fy([cy([pe(`case e of
  Just x -> x *> push Nothing
  _ -> (run2_ [ gain_ 0.15 [ sinOsc 440.0 bangOn ] ]
         >>= Just >>> push`)])])),result:B(wt(n)(u)(function(i){return l(xa)(void 0)})(function(i){return function(o){return yt(i)([Nt(lt)(.15)([lf(kc)(440)(vt())])])}})),next:Za(R(D))(C(V))(n)(a)})}}}};var UT=Wf;var WT=function(){return function(t){return t}},qT=function(){return function(t){return t}};var ly=function(){function t(){}return t.value=new t,t}();var ZT={attr:function(t){return function(r){return b({key:"height",value:q(r)})}}};var _y=function(){function t(){}return t.value=new t,t}();var zT={attr:function(t){return function(r){return b({key:"width",value:q(r)})}}};var py=function(t){return function(r){return new M(X("canvas")(t)(U(r)))}};var sy=function(){function t(){}return t.value=new t,t}();var my={attr:function(t){return function(r){return b({key:"@self@",value:st(r)})}}};function Hm(t){return function(){return t.getContext("2d")}}function hp(t){return function(r){return function(){t.fillStyle=r}}}function Um(t){return function(){t.beginPath()}}function Wm(t){return function(){t.fill()}}function vy(t){return function(r){return function(){t.arc(r.x,r.y,r.radius,r.start,r.end,r.useCounterClockwise)}}}function qm(t){return function(r){return function(){t.fillRect(r.x,r.y,r.width,r.height)}}}var PB=function(){return 2*dc}(),Pl=function(t){return{o:t.value0+.04,n:t.value1,t:ii}};var IB=function(){return d.value}(),Il=function(t){return function(r){return function(e){return function(n){return l(t)(Lo(r)(Hn)({p:[e,n],o:0,d:16}))}}}},RB=function(t){return function(r){return l(t)(In(r)(Hn)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:18}))}},NB=function(t){return function(r){return l(t)(In(r)(Hn)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:24}))}};var Zm=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return function(o){return function(p){return function(s){return up(t)(n)(a)([Je(r)(u)(i)([Td(e)(o)(p)(s)])])}}}}}}}}}},VT=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return function(o){return function(p){return function(s){return up(t)(n)(a)([Je(r)(u)(i)([Ed(e)(o)(p)(s)])])}}}}}}}}}},LB=function(t){return function(r){return function(e){return function(n){return l(t)(Tl(r)(Hn)({p:[e,n],o:0,d:16}))}}}},GT=400,Dy=jr(GT),BB=function(){return jt(uu)(GT)+"px"}(),JT=600,dy=jr(JT),HB=function(){return jt(uu)(JT)+"px"}(),UB={pluck0:"https://freesound.org/data/previews/493/493016_10350281-lq.mp3",pluck1:"https://freesound.org/data/previews/141/141524_2558140-lq.mp3",strum0:"https://freesound.org/data/previews/234/234738_3635427-lq.mp3"},jT=function(t){return function(r){return function(e){return Dr({reflectType:function(){return"<section>@ex1@</section>"}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))(d.value)(IB)({ex1:B(eo()()(Wr({reflectSymbol:function(){return"canvas"}})(qr(_e))(Wr({reflectSymbol:function(){return"slider"}})(qr(_e))(Wr({reflectSymbol:function(){return"startStop"}})(qr(ro(su()(Wr({reflectSymbol:function(){return"loading"}})(qr(_e))(Wr({reflectSymbol:function(){return"start"}})(qr(_e))(Wr({reflectSymbol:function(){return"stop"}})(qr(_e))(Qn)()())()())()()))))(Qn)()())()())()())(fe({reflectSymbol:function(){return"canvas"}})()()(fe({reflectSymbol:function(){return"slider"}})()()(Qu({reflectSymbol:function(){return"startStop"}})()()()(fe({reflectSymbol:function(){return"loading"}})()()(fe({reflectSymbol:function(){return"start"}})()()(fe({reflectSymbol:function(){return"stop"}})()()(Xn)()())()())()())(Xn)()())()())()())(d.value)(function(n){return function(a){var u=I(R(D))(l(C(V))(void 0))(a.startStop.start),i=function(o){return function(p){return function(s){var f=_(g)(function(m){return new et(m.acTime,m.value)})(xl(o)(pu(a.slider)));return[Dm(vm(Et(ht()(J(J(Ct)(mm)()()()({reflectSymbol:function(){return"fftSize"}}))(sm)()()()({reflectSymbol:function(){return"cb"}})))(gt()())))({cb:function(m){return function(){return kn(new F(m))(s)(),kn(W.value)(s)}},fftSize:em.value})(l(de)(Wa(aa(tu)(p)(I(R(w))(vt())(_(g)(function(){var m=Da()(qe),v=ea(On)(Va(0)(.96)(100)(1.04));return function(c){return m(Pl(v(c)))}}())(f))))(function(m){return function(v){return Du(function(c){return Nt(lt)(1)([m,up(Ad(Et(ht()(J(J(Ct)(Nh)()()()({reflectSymbol:function(){return"maxDelayTime"}}))(cd)()()()({reflectSymbol:function(){return"delayTime"}})))(gt()())))({maxDelayTime:2.5,delayTime:1})(_(g)(function(){var h=Tl()(qe),ut=ea(On)(Va(0)(.5)(100)(2.45));return function(dt){return h(Pl(ut(dt)))}}())(f))([Je(lt)(.4)(_(g)(function(){var h=In()(qe),ut=ea(On)(Va(0)(.6)(100)(.9));return function(dt){return h(Pl(ut(dt)))}}())(f))([m])]),Zm(un)(lt)(vu)(.15)(O(E(w)))(.7)(O(E(w)))(1500)(Il(C(G))()(1500)(3e3))([Du(function(h){return Je(lt)(1)(RB(C(G))())([Zm(un)(lt)(vu)(.4)(O(E(w)))(.5)(O(E(w)))(3e3)(Il(C(G))()(3e3)(100))([c,h])])})]),Zm(un)(lt)(vu)(.29)(_(g)(function(){var h=Tl()(qe),ut=ea(On)(Va(0)(.1)(100)(.4));return function(dt){return h(Pl(ut(dt)))}}())(f))(.85)(O(E(w)))(2e3)(Il(C(G))()(2e3)(5e3))([Du(function(h){return Nt(lt)(1)([Zm(un)(lt)(vu)(.6)(_(g)(function(){var ut=Tl()(qe),dt=ea(On)(Va(0)(.8)(100)(.3));return function(fr){return ut(Pl(dt(fr)))}}())(f))(.6)(O(E(w)))(3500)(Il(C(G))()(3500)(100))([c,Du(function(ut){return Je(lt)(1)(NB(C(G))())([VT(un)(lt)(kd)(.75)(_(g)(function(){var dt=Tl()(qe),fr=ea(On)(Va(0)(.9)(100)(.1));return function(Zt){return dt(Pl(fr(Zt)))}}())(f))(.6)(O(E(w)))(4e3)(Il(C(G))()(4e3)(200))([h,ut]),VT(un)(lt)(kd)(.75)(LB(C(G))()(.75)(.2))(.55)(O(E(w)))(200)(Il(C(G))()(200)(4e3))([m])])})])])})])])})}})))]}}};return zr([py(I(R(D))(Cn(Bt)(E(D))(l(C(V)))([tt(zT)(_y.value)(HB),tt(ZT)(ly.value)(BB),tt(_g)(Xt.value)("width: 100%;"),tt(my)(sy.value)(function(){var o=ce(w)(te)(function(p){return function(){var f=Hm(p)();return hp(f)("black")(),qm(f)({width:dy,height:Dy,x:0,y:0})(),void 0}});return function(p){return o(FD(p))}}())]))(_(g)(function(o){return tt(my)(sy.value)(function(){var p=ce(w)(te)(function(s){return function(){var m=Hm(s)();return hp(m)("black")(),qm(m)({width:dy,height:Dy,x:0,y:0})(),hp(m)("rgba(255,255,255,0.2)")(),Vl(o)(function(v){return function(){return Um(m)(),vy(m)({end:PB,radius:v.value1*40,start:0,x:v.value0.x*dy,y:v.value0.y*Dy,useCounterClockwise:!1})(),Wm(m)()}})()}});return function(s){return p(FD(s))}}())})(a.canvas)))([]),Ho(Cn(Bt)(E(D))(l(C(V)))([tt(si)(Bo.value)("range"),tt(Sc)(Vi.value)("0"),tt(Tc)(zi.value)("100"),tt(Fc)(Ji.value)("1"),tt(Oc)(ji.value)("50"),tt(lg)(Xt.value)("width: 100%;"),tt(xc)(Gi.value)(ne(function(){var o=ce(w)(te)(Af(Gn)(rf)(n.slider)),p=Vn(wa)(Yf);return function(s){return o(p(wl(s)))}}()))]))([]),Mn(xe(Bt)(E(D))([l(C(V))(tt(qf)(Xt.value)("width:100%; padding:1.0rem;")),Cn(Bt)(E(D))(_(g)(function(){var o=tt(ye)(De.value);return function(p){return o(ne(T(p)))}}()))([K(g)(a.startStop.loading)(l(w)(void 0)),Gr(g)(a.startStop.stop)(function(o){return Q(ot)(o)(Q(ot)(t(l(w)(void 0)))(n.startStop.start(void 0)))}),Gr(g)(Ln(Vt(V))(I(R(D))(l(C(V))(l(w)(void 0)))(_(g)(function(o){return o.value0})(e)))(K(g)(u)(rt(nt))))(function(o){return function(){o(),n.startStop.loading(void 0)();var s=ie(W.value)(),f=No(P(Ue)(va(He))(function(m){return P(Ue)(io(He)(m))(function(v){return P(Ue)(_(Hi)(qT())(wE(Wn)(UT)(Tt(m))(WT()(UB))))(function(c){return P(Ue)(Ae(He)(F_(0)(5e4)))(function(h){var ut=Kf(SD(Aa(w_(c.pluck0))(jf(Kv(Yv()(c))))))({newSeed:Jf(h),size:4});return Ae(He)(function(){var fr=Nn(bo)(w)(function(pt){return function(){var Fr=no(),ir=no();return{x:Fr,y:ir}}})(An(0)(127))(),Zt=yt(m)(i(m)(ut)(s))(),Kr=$t(Cc)(function(pt){return function(){var Fr=Ee(s)();return jn(w)(te)(Fr)(function(ir){return function(){var Uo=mp(ir)(),co=_(S)(function(){var Xi=a_(fr),hu=_($r)(function(yn){return function(vi){return vi/255}(yn)});return function(yn){return Xi(hu(yn))}}())(xm(Sm)(Uo))();return n.canvas(co)(),void 0}})()}})(),re=Q(ot)(Q(ot)(Q(ot)(Zt)(v))(Pn(se)(m)))(Kr);return n.startStop.stop(re)(),re})})})})}))();return t(function(){return n.startStop.start(void 0)(),li(Wi(f))()})(),void 0}})])]))([mn(xe(Bt)(E(D))([_(g)(T("Turn off"))(a.startStop.stop),_(g)(T("Turn on"))(u),_(g)(T("Loading..."))(a.startStop.loading)]))])])}}))})}}};var qB=function(){return d.value}(),XT=function(t){return function(r){return function(e){return function(n){var a=za(t)(e);return It({reflectType:function(){return`<div>
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
</div>`}})()()(H()(vn()(Y)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"ex"}})({reflectSymbol:function(){return"ex"}}))(qB)({next:Za(R(D))(C(V))(n)(Q(ot)(r(_f.value))(bn)),ex:B(jT(a)(r)(n))})}}}};var zB=function(){return d.value}(),QT=function(t){return function(r){return function(e){return function(n){return It({reflectType:function(){return`<div>
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
</div>`}})()()(vn()(Y)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))(zB)({next:l(C(V))(tt(ye)(De.value)(ne(T(Q(ot)(r(ym.value))(bn)))))})}}}};var GB=function(){return d.value}(),KT=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(H()(H()(Y)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}}))(GB)({txt:B(pe(`\\ctx buf -> run2 ctx
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
  ]`)),cancel:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([Nt(lt)(1)([sr(Er)(a)(xe(Bt)(E(w))([vt(),Xu(1e3)(l(C(G))(Da()(Hn)({p:nu(yi)(K($r)(An(0)(60))([1,1.2,1,.8])),o:1.5,d:30}))),Xu(3e3)(l(C(G))(Da()(Ch)({o:3.5})))]))])])}}))})}}};var jB=function(){return d.value}(),YT=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2>Envelope</h2>
  <p>The <code>AudioEnvelope</code> parameter corresponds to the Web Audio API's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setValueCurveAtTime"><code>setValueCurveAtTime</code></a> function and sets an envelope <code>p</code> over the duration <code>d</code> starting at time <code>o</code>.</p>
  <pre><code>~txt~</code></pre>
  ~envelope~
  </section>
`}})()()(H()(H()(Y)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}}))(jB)({txt:B(pe(`\\ctx buf -> run2 ctx
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
  ]`)),envelope:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([Nt(lt)(1)([sr(Er)(a)(xe(Bt)(E(w))([vt(),Xu(1e3)(l(C(G))(Da()(Hn)({p:nu(yi)(K($r)(An(0)(60))([1,1.2,1,.8])),o:1.5,d:30})))]))])])}}))})}}};var QB=function(){return d.value}(),tS=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"numericEx"}})({reflectSymbol:function(){return"numericEx"}}))(d.value)(QB)({numericEx:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([Nt(lt)(1)([sr(Er)(a)(mi(R(w))(vt())(function(){return mi(R(w))(Xu(1e3)(mi(R(w))(l(C(G))(Da()(qe)({n:1,o:1,t:nd})))(function(){return l(C(G))(Da()(qe)({n:1.3,o:2,t:ii}))})))(function(){return Xu(2500)(mi(R(w))(l(C(G))(Da()(qe)({n:1,o:2.5,t:nd})))(function(){return l(C(G))(Da()(qe)({n:.7,o:3.5,t:gh}))}))})}))])])}}))})}}};var YB=function(){return d.value}(),rS=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"suddenEx"}})({reflectSymbol:function(){return"suddenEx"}}))(d.value)(YB)({suddenEx:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([Nt(lt)(1)([sr(Er)(a)(xe(Bt)(E(w))([vt(),Xu(1500)(l(C(G))(Da()(Ah)({n:1.4})))]))])])}}))})}}};var rH=function(){return d.value}(),eS=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(H()(Y)({reflectType:function(){return"unitEx"}})({reflectSymbol:function(){return"unitEx"}}))(rH)({unitEx:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([sr(Er)(a)(xe(Bt)(E(w))([vt(),l(C(G))(Da()(dh(Pi)(Pi))(Dh(Nt(lt)(1)([bm(cm)(1)(vt()),Nt(lt)(.2)([ml(lm)(100)([np(al)(50)(vt())])])]))))]))])}}))})}}};var nH=function(){return d.value}(),nS=function(t){return function(r){return function(e){return function(n){var a=Q(ot)(r(pp.value))(bn),u=za(t)(e);return It({reflectType:function(){return`<div>
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
</div>`}})()()(H()(H()(vn()(H()(H()(H()(Y)({reflectType:function(){return"unit"}})({reflectSymbol:function(){return"unit"}}))({reflectType:function(){return"sudden"}})({reflectSymbol:function(){return"sudden"}}))({reflectType:function(){return"numeric"}})({reflectSymbol:function(){return"numeric"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}}))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}}))(nH)({sudden:B(rS(u)(r)(n)),numeric:B(tS(u)(r)(n)),envelope:B(YT(u)(r)(n)),cancel:B(KT(u)(r)(n)),unit:B(eS(u)(r)(n)),next:Za(R(D))(C(V))(n)(a)})}}}};var uH=function(){return d.value}(),aS=function(t){return function(r){return function(e){return function(n){return It({reflectType:function(){return`<div>
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
</div>`}})()()(vn()(Y)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))(uH)({next:l(C(V))(tt(ye)(De.value)(ne(T(Q(ot)(r(Am.value))(bn)))))})}}}};var iH=function(){return d.value}(),uS=function(t){return function(r){return function(e){return function(n){return It({reflectType:function(){return`<div>
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
</div>`}})()()(vn()(Y)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))(iH)({next:l(C(V))(tt(ye)(De.value)(ne(T(Q(ot)(r(pf.value))(bn)))))})}}}};var fH=function(){return d.value}(),oS=function(t){return function(r){return function(e){return function(n){return Dr({reflectType:function(){return`<div>
  <h1>Imperative API</h1>

  <h2>Like JavaScript, but PureScript</h2>
  <p>
    If you're coming from the JavaScript or TypeScript world, or if you're a fan of monadic <code>do</code> notation, you may enjoy building things step-by-step rather than constructing large declarative structures. If you're that sort of person, this section is for you!
  </p>

  <h2>Parting shot</h2>
  <p>Thanks for checking out ocarina! We want it to be the most ergonomimc, expressive, and performant Web Audio API on your side of the Mississippi. It certainly is for me, and as I'm in Finland, I'm on <i>both sides</i> of the Mississippi, so you can't beat that! If you have any questions, comments, concerns or would just like to say "hi!", please check out the <a href="https://github.com/mikesol/purescript-ocarina">Ocarina GitHub Repo</a> or the <a href="https://purescript.org/chat">PureScript Discord's music channel</a>. Now go out there and play some ocarina!</p>
</div>`}})({reflectType:function(){return"~"}})()()(Y)(d.value)(fH)({})}}}};var _H=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst)
import QualifiedDo.Alt as OneOf
import Data.Tuple.Nested ((/\\))
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (switcher, text, text_)
import Deku.Core (Domable)
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
    :: forall lock payload
     . Maybe BrowserAudioBuffer
    -> Domable Effect lock payload
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
`,pH=gu(function(t){return Ut(function(r){return $t(t)(function(e){return function(){var a=no();return r(e(a))()}})})}),sH=function(){return d.value}(),mH="https://freesound.org/data/previews/339/339810_5121236-lq.mp3",iS=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<section>
  <h2>Hello subgraph</h2>

  <p>Subgraphs have the type <code>Event (Event (Channel outputChannels lock payload))</code>. Streaming audio is a data type with two constructors: <code>sound</code> to create a subgraph and <code>silence</code> to turn it off. The inner event listens for sound/silence, and the outer event adds subgraphs to the scene. You can create as many subgraphs as you like: ocarina automatically frees up resources when you send the <code>silence</code> event. Note that, once you turn a subraph off with <code>silence</code>, you can't turn it back on again. In this case, just create a new subgraph.</p>

  <p>Here's a simple subgraph that is connected to a slider. As you slide the slider, new nodes are provisioned. Each one has a pseudo-random pitch.</p>

  <pre><code>@txt@</code></pre>
  @ex1@

</section>
`}})({reflectType:function(){return"@"}})()()(H()(H()(Y)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))(d.value)(sH)({txt:B(pe(_H)),ex1:B(eo()()(Wr({reflectSymbol:function(){return"slider"}})(qr(_e))(Wr({reflectSymbol:function(){return"startStop"}})(qr(ro(su()(Wr({reflectSymbol:function(){return"loading"}})(qr(_e))(Wr({reflectSymbol:function(){return"start"}})(qr(_e))(Wr({reflectSymbol:function(){return"stop"}})(qr(_e))(Qn)()())()())()()))))(Qn)()())()())(fe({reflectSymbol:function(){return"slider"}})()()(Qu({reflectSymbol:function(){return"startStop"}})()()()(fe({reflectSymbol:function(){return"loading"}})()()(fe({reflectSymbol:function(){return"start"}})()()(fe({reflectSymbol:function(){return"stop"}})()()(Xn)()())()())()())(Xn)()())()())(d.value)(function(n){return function(a){var u=I(R(D))(l(C(V))(void 0))(a.startStop.start),i=Zi(g)(et.create)(pH)(ju(Vt(G))(function(p){return function(s){return s+1|0}})(pu(a.slider))(0)),o=function(p){return[Nt(lt)(1)([_s(_(g)(function(s){return xe(Bt)(E(w))([l(C(G))(mh(aa(tp(Et(ht()(J(J(Ct)(Oh)()()()({reflectSymbol:function(){return"playbackRate"}}))(Q_)()()()({reflectSymbol:function(){return"buffer"}})))(gt()())))({buffer:p,playbackRate:.7+Qa(s)*2})(vt()))),Xu(5e3)(l(C(G))(vh))])})(i))])]};return zr([zr([pe("Slide me!"),Ho(Cn(Bt)(E(D))(l(C(V)))([tt(si)(Bo.value)("range"),tt(Sc)(Vi.value)("0"),tt(Tc)(zi.value)("100"),tt(Fc)(Ji.value)("1"),tt(Oc)(ji.value)("50"),tt(xc)(Gi.value)(ne(T(n.slider(void 0))))]))([])]),Mn(Cn(Bt)(E(D))(_(g)(function(){var p=tt(ye)(De.value);return function(s){return p(ne(T(s)))}}()))([K(g)(a.startStop.loading)(l(w)(void 0)),Gr(g)(a.startStop.stop)(function(p){return Q(ot)(p)(Q(ot)(t(l(w)(void 0)))(n.startStop.start(void 0)))}),Gr(g)(Ln(Vt(V))(I(R(D))(l(C(V))(l(w)(void 0)))(_(g)(function(p){return p.value0})(e)))(K(g)(u)(rt(nt))))(function(p){return function(){p(),n.startStop.loading(void 0)();var f=No(P(Ue)(va(He))(function(m){return P(Ue)(io(He)(m))(function(v){return P(Ue)(Tt(m)(mH))(function(c){return Ae(He)(function(){var ut=Tm(o(c))(),dt=Q(ot)(Q(ot)(ut)(v))(Pn(se)(m));return n.startStop.stop(dt)(),dt})})})}))();return t(function(){return n.startStop.start(void 0)(),li(Wi(f))()})(),void 0}})]))([mn(xe(Bt)(E(D))([_(g)(T("Turn off"))(a.startStop.stop),_(g)(T("Turn on"))(u)]))])])}}))})}}};var DH=function(){return d.value}(),cS=function(t){return function(r){return function(e){return function(n){var a=za(t)(e);return It({reflectType:function(){return`<div>
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
</div>`}})()()(H()(H()(Y)({reflectType:function(){return"suby"}})({reflectSymbol:function(){return"suby"}}))({reflectType:function(){return"appl"}})({reflectSymbol:function(){return"appl"}}))(DH)({appl:B(Em("\u{1F44F}")(n)(a)(function(u){return Tt(u)("https://freesound.org/data/previews/277/277021_1402315-lq.mp3")})(function(u){return function(i){return yt(u)([Nt(lt)(1)([sr(Er)(i)(vt())])])}})),suby:B(iS(a)(r)(n))})}}}};var HBt=function(t){return t},UBt={Coercible0:function(){}},bH=function(){var t=function(r){var e=function(n){if(n instanceof cp)return zr(l(de)(nn(XT(r.setCancellation)(r.setPage))));if(n instanceof _f)return zr(l(de)(nn(HT(r.setCancellation)(r.setPage))));if(n instanceof lp)return zr(l(de)(nn(NT(r.setCancellation)(r.setPage))));if(n instanceof fp)return zr(l(de)(nn(I0(r.setCancellation)(r.setPage))));if(n instanceof ym)return zr(l(de)(nn(uS(r.setCancellation)(r.setPage))));if(n instanceof pf)return zr(l(de)(nn(FT(r.setCancellation)(r.setPage))));if(n instanceof _p)return zr(l(de)(nn(nS(r.setCancellation)(r.setPage))));if(n instanceof pp)return zr(l(de)(nn(dT(r.setCancellation)(r.setPage))));if(n instanceof Am)return zr(l(de)(nn(oS(r.setCancellation)(r.setPage))));if(n instanceof zE)return zr(l(de)(nn(QT(r.setCancellation)(r.setPage))));if(n instanceof sp)return zr(l(de)(nn(cS(r.setCancellation)(r.setPage))));if(n instanceof VE)return zr(l(de)(nn(aS(r.setCancellation)(r.setPage))));throw new Error("Failed pattern match at Ocarina.Example.Docs (line 145, column 5 - line 145, column 76): "+[n.constructor.name])};return e(r.page)};return vg(mg(new Al(cp.value)))(function(r){var e=ju(Vt(V))(function(n){if(n instanceof Al)return function(a){return{prevPage:new F(a.curPage),curPage:n.value0,cancel:a.cancel,pageChange:!0}};if(n instanceof Od)return function(a){return{cancel:n.value0,pageChange:!1,curPage:a.curPage,prevPage:a.prevPage}};throw new Error("Failed pattern match at Ocarina.Example.Docs (line 57, column 15 - line 59, column 83): "+[n.constructor.name])})(r.value1)({prevPage:W.value,curPage:cp.value,cancel:l(w)(void 0),pageChange:!0});return zr([zr(_($r)(function(n){return lD([fD(I(R(D))(Cn(Bt)(E(D))(l(C(V)))([tt(ye)(De.value)(ne(T(r.value0(new Al(n.value0))))),tt(pg)(Xt.value)("cursor:pointer;")]))(_(g)(function(a){return tt(ye)(De.value)(ne(T(function(){return a.cancel(),r.value0(new Al(n.value0))()})))})(l_(Ou(D))(function(){var a=lu(Xa);return function(u){return a(function(i){return i.pageChange}(u))}}())(e))))([pe(n.value1.value0)]),zf(l(C(V))(tt(bs)(Xt.value)(function(){return n.value1.value1?"":"display:none;"}())))([pe(" | ")])])})([new et(cp.value,new et("Home",!0)),new et(_f.value,new et("Hello world",!0)),new et(lp.value,new et("Array, fan, and fix",!0)),new et(fp.value,new et("Audio units",!0)),new et(pf.value,new et("Events",!0)),new et(_p.value,new et("Parameters",!0)),new et(pp.value,new et("State",!0)),new et(sp.value,new et("Subgraphs",!1))])),ig(hr)(function(n){return t({page:n.curPage,setPage:function(a){return r.value0(Al.create(a))},setCancellation:function(a){return r.value0(Od.create(a))}})})(l_(Ou(D))(function(n){return n.pageChange})(e))])})}(),WBt=function(t){return{page:t,setPage:Pt(rn(Le(Ne))),setCancellation:Pt(rn(Le(Ne)))}},qBt=hC(bH);export{HBt as TopLevelSg,qBt as main,UBt as newtypeTopLevelSg_,WBt as p2tl,bH as scene};
