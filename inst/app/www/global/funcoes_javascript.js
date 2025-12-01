var openTab = function(tabName){
  $('a', $('.sidebar')).each(function() {
    if(this.getAttribute('data-value') == tabName) {
      this.click()
    };
  });
}

function atualizarTopCardEOverlay() {
  const divPlaceholder = document.querySelector(".div-placeholder");
  const baseAltura     = divPlaceholder ? divPlaceholder.clientHeight : 0;

  /* Header do #card-filters (filho direto) + header de cards cujo id termina em -card-visualizations (filho direto) */
  document
    .querySelectorAll('#card-filters > .card-header, [id$="-card-visualizations"] > .card-header')
    .forEach(header => {
      header.style.top = `${baseAltura + 87}px`;
    });

  /* Overlays */
  document
    .querySelectorAll(".div-corta-card")
    .forEach(overlay => {
      overlay.style.top = `${baseAltura + 62}px`;
    });
}

// Atualiza tudo de uma vez
function atualizarTudo() {
  //applySidebarMini();
  atualizarTopCardEOverlay();
}

// Gatilhos
$(document).on("shiny:inputchanged", atualizarTudo);
window.addEventListener("load", atualizarTudo);
window.addEventListener("resize", atualizarTudo);


function toggleNavbarTitle() {
  const banner = document.getElementById('custom-banner');
  const bannerHeight = banner ? banner.offsetHeight : 0;
  const screenWidth = window.innerWidth;

  if (screenWidth <= 1200) {
    // Força a classe em telas pequenas
    document.body.classList.add('navbar-show-title');
  } else {
    // Comportamento padrão para telas maiores
    if (window.scrollY > bannerHeight - 20) {
      document.body.classList.add('navbar-show-title');
    } else {
      document.body.classList.remove('navbar-show-title');
    }
  }
}

window.addEventListener('load', toggleNavbarTitle);
window.addEventListener('scroll', toggleNavbarTitle);
window.addEventListener('resize', toggleNavbarTitle);

function ajustaFonteNomeHospital() {
  var elements = document.querySelectorAll('.fonte-nome-hospital');
  elements.forEach(function(el) {
    var parentWidth = el.parentElement.clientWidth;
    el.style.setProperty('font-size', '45px');
    while((el.scrollWidth > parentWidth || el.scrollHeight > el.clientHeight) && parseInt(window.getComputedStyle(el).fontSize) > 12){
      var currentSize = parseInt(window.getComputedStyle(el).fontSize);
      el.style.setProperty('font-size', (currentSize - 1) + 'px', 'important');
    }
  });
}

$(document).on("shiny:inputchanged", ajustaFonteNomeHospital);
window.addEventListener('load', ajustaFonteNomeHospital);
window.addEventListener('resize', ajustaFonteNomeHospital);



Shiny.addCustomMessageHandler("accordion-set", function(message) {
  const id = message.id;
  const state = message.state; // "open" ou "closed"

  const container = document.getElementById(id + "-header-container");
  const icon = document.getElementById(id + "-icon");
  const body = document.getElementById(id + "-body");

  if (!container || !icon || !body) return;

  const isClosed = container.classList.contains("closed");

  if (state === "closed" && !isClosed) {
    container.classList.add("closed");
    icon.classList.add("closed");
    body.style.display = "none";
    Shiny.setInputValue(id + "_click", "closed", {priority: "event"});
  }
  if (state === "open" && isClosed) {
    container.classList.remove("closed");
    icon.classList.remove("closed");
    body.style.display = "block";
    Shiny.setInputValue(id + "_click", "open", {priority: "event"});
  }
});


function syncWidth(id){
  var sel = $('#' + id)[0];
  if(!sel) return;
  var s = sel.selectize;
  if(!s) return;

  function setWidth(){
    var w = s.$control.outerWidth(); // ajuste para teste
    setTimeout(function(){
      s.$dropdown.css({
        width: w + 'px',
        minWidth: w + 'px',
        maxWidth: w + 'px'
      });
    }, 0);  // Aumenta o delay para 100ms
  }
  s.on('dropdown_open', setWidth);
  $(window).on('resize', setWidth);
}

$(document).on('shiny:connected shiny:inputchanged', function(e){
  if(e.name === 'input_categoria' || e.type === 'shiny:connected'){
    syncWidth('input_categoria');
  }
});


$(document).ready(function() {
      // seleciona o input correspondente ao valor 'agrupado'
      var el = $('input[name=\"input_desejo_visualizar\"][value=\"agrupado\"]');
      el.prop('disabled', true); // desabilita o radio
      el.closest('label').css({
        'color': 'gray',
        'cursor': 'not-allowed'
      }); // estiliza o label
    });


document.addEventListener("wheel", function(e) {
  const dropdown = e.target.closest(".vscomp-dropbox");
  if (dropdown) {
    dropdown.classList.add("vs-scrolling");
    clearTimeout(dropdown._scrollTimer);
    dropdown._scrollTimer = setTimeout(() => dropdown.classList.remove("vs-scrolling"), 150);
  }
}, { passive: true });


if (window.VirtualSelect) {
  VirtualSelect.prototype.scrollToActiveItem_ = function(){};
}


$(document).on('click', '.filter-icon', function(e) {
  e.stopPropagation();
  var menu = $(this).next('.filter-dropdown');
  var isOpen = menu.hasClass('show');
  $('.filter-dropdown.show').not(menu).each(function() {
    var that = $(this);
    that.removeClass('show').addClass('hide');
    setTimeout(function() { that.hide().removeClass('hide'); }, 150);
  });
  if (!isOpen) {
    menu.show(0, function() {
      $(this).addClass('show');
    });
  } else {
    menu.removeClass('show').addClass('hide');
    setTimeout(function() { menu.hide().removeClass('hide'); }, 150);
  }
});

$(document).on('click', function(e) {
  if (!$(e.target).closest('.filter-dropdown, .filter-icon').length) {
    $('.filter-dropdown.show').each(function() {
      var menu = $(this);
      menu.removeClass('show').addClass('hide');
      setTimeout(function() { menu.hide().removeClass('hide'); }, 150);
    });
  }
});


$(function() {
  $('.navbar .dropdown-toggle').off('click');
  let hideTimerLevel1 = null;
  let hideTimerLevel2 = null;

  // --- Nível 1 ---
  $('.navbar .dropdown').on('mouseenter', function() {
    clearTimeout(hideTimerLevel1);
    $(this).addClass('show');
    $(this).children('.dropdown-menu').addClass('show');
  }).on('mouseleave', function(e) {
    const dropdown = $(this);
    const related = e.relatedTarget;
    if (related && ($(related).closest('.dropdown').is(dropdown) ||
                    $(related).closest('.dropdown-submenu').length)) return;
    hideTimerLevel1 = setTimeout(function() {
      dropdown.removeClass('show');
      dropdown.children('.dropdown-menu').removeClass('show');
      dropdown.find('.dropdown-submenu').removeClass('show')
              .children('.dropdown-menu').removeClass('show');
    }, 180);
  });

  // --- Nível 2 ---
  $('.navbar').on('mouseenter', '.dropdown-submenu', function() {
    clearTimeout(hideTimerLevel2);
    $(this).siblings('.dropdown-submenu').removeClass('show')
           .children('.dropdown-menu').removeClass('show');
    $(this).addClass('show');
    $(this).children('.dropdown-menu').addClass('show');
  }).on('mouseleave', '.dropdown-submenu', function(e) {
    const submenu = $(this);
    const related = e.relatedTarget;
    if (related && ($(related).closest('.dropdown-submenu').is(submenu) ||
                    $(related).closest('.dropdown').length)) return;
    hideTimerLevel2 = setTimeout(function() {
      submenu.removeClass('show');
      submenu.children('.dropdown-menu').removeClass('show');
    }, 180);
  });

  // Clique em itens de segundo nível: seta input$navmenu e abre o bloco no sidebar
  $('.navbar').on('click', '.dropdown-menu a[data-value]', function(e) {
    e.preventDefault();
    e.stopPropagation();

    const val = $(this).attr('data-value');  // ex: 'indicadores_perfil_sociodemografico-porc_nvm_escolaridade'

    // extrai tudo antes do último hífen como nome do bloco pai
    const parent = val.includes('-') ? val.substring(0, val.lastIndexOf('-')) : val;

    if (typeof Shiny !== 'undefined') {
      Shiny.setInputValue('navmenu', val, {priority: 'event'});
    }

    // Abre o tab correspondente no sidebar
    const $side = $('.sidebar a[data-value=\"' + parent + '\"]');
    if ($side.length) { $side[0].click(); }

    // Fecha todos os menus abertos da navbar
    $('.navbar .dropdown.show, .navbar .dropdown-submenu.show')
      .removeClass('show')
      .children('.dropdown-menu').removeClass('show');

    return false;
  });
});
