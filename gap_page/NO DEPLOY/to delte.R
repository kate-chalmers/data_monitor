@import url('https://fonts.googleapis.com/css?family=Noto Sans:400,500,600,700,800,900');

* {
  font-family: 'Noto Sans', sans-serif;
  font-size: 1.5rem;
  color: #586179;
}

.type_1-circle-fill > svg > path {
  color: #0F8554;
}

.type_2-circle-fill > svg > path {
  color: goldenrod;
}

.type_3-circle-fill > svg >path {
  color: #CF597E;
}

.type_three-dots > svg > path {
  color: #999999;
}

.card {
  border: solid 1.5px lightgrey;
  height: 150px;
  width: 90%;
  padding: 0 !important;
  background: #FFFFFF !important;
    overflow: hidden;
  border-radius: 25px;
  display: inline-block;
}

.card-inner {
  position: relative;
  width: 100%;
  height: 100%;
  transition: transform 1.5s;
  transform-style: preserve-3d;
}

.card:hover .card-inner {
  transform: rotateY(180deg);
  cursor: pointer;
}

.card-front,
.card-back {
  height: 100%;
  backface-visibility: hidden;
  display: flex;
  flex-direction: column;
  justify-content: space-between;
  border-radius: 25px;
  padding: 5px;
}

.card-front {
  background: #FFFFFF;
    line-height: 100%;
}

.card-back {
  background: #FFFFFF;
    transform: rotateY(180deg);
  padding: 0px;
}

.card-front .plot-container {
  bottom: 0;
  height: 50%;
}

.container .folder .slide {
  width: 100%;
}

.container .folder .slide.slide1 {
  position: relative;
  display: flex;
  justify-content: center;
  align-items: center;
  z-index: 2;
  transform: none;
  transition: none;
}

.container .folder .slide.slide2 {
  max-height: 0;
  opacity: 0;
  overflow: hidden;
  transition: all 0.8s ease-in-out;
  background: #f8f8f8;
    border-radius: 20px;
  padding: 20px;
}

.container .folder:hover .slide.slide2 {
  max-height: 300px;
  opacity: 1;
}

.container .folder .slide.slide2 .content p {
  margin: 0;
  padding: 0;
  text-align: center;
  color: #414141;
}

.container .folder .slide.slide2 .content h3 {
  margin: 0 0 10px 0;
  padding: 0;
  font-size: 24px;
  text-align: center;
  color: #414141;
}
