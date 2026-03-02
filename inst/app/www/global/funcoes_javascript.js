var openTab = function(tabName){
  $('a', $('.sidebar')).each(function() {
    if(this.getAttribute('data-value') == tabName) {
      this.click()
    };
  });
}

$(document).on('hidden.bs.modal', '.modal', function () {
    // Garante que o body recupere o scroll e remova classes de trava do Bootstrap
    $('body').css({
        'overflow': 'auto',
        'height': 'auto',
        'padding-right': '0'
    }).removeClass('modal-open');

    // Remove backdrops residuais que o Shiny às vezes deixa para trás
    $('.modal-backdrop').remove();
});

// Impede que o clique nos botões redondos dê o "pulo" no href="#"
$(document).on('click', '.circle-btn', function(e) {
    e.preventDefault();
});


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
      overlay.style.top = `${baseAltura + 63}px`;
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


document.addEventListener("wheel", function(e) {
  const dropdown = e.target.closest(".vscomp-dropbox");
  if (dropdown) {
    dropdown.classList.add("vs-scrolling");
    clearTimeout(dropdown._scrollTimer);
    dropdown._scrollTimer = setTimeout(() => dropdown.classList.remove("vs-scrolling"), 150);
  }
}, { passive: true });



// Botão de filtros dos gráficos
// 1. Abrir/Fechar ao clicar na DIV
$(document).on('click', '.div-botao-filtros', function(e) {

  // Proteção: Se clicou dentro do conteúdo do dropdown (checkboxes), não faz nada
  if ($(e.target).closest('.filter-dropdown').length) {
    return;
  }

  e.stopPropagation();

  var $botao = $(this); // Guardamos a referência da div clicada
  var menu = $botao.find('.filter-dropdown');
  var isOpen = menu.hasClass('show');

  // Fecha outros dropdowns abertos e REMOVE a classe active deles
  $('.filter-dropdown.show').not(menu).each(function() {
    var that = $(this);
    that.closest('.div-botao-filtros').removeClass('active'); // Remove brilho do outro botão
    that.removeClass('show').addClass('hide');
    setTimeout(function() { that.hide().removeClass('hide'); }, 150);
  });

  if (!isOpen) {
    // ABRINDO: Adiciona classe 'active' no botão para manter o brilho
    $botao.addClass('active');

    menu.show(0, function() {
      $(this).addClass('show');
    });
  } else {
    // FECHANDO: Remove classe 'active'
    $botao.removeClass('active');

    menu.removeClass('show').addClass('hide');
    setTimeout(function() { menu.hide().removeClass('hide'); }, 150);
  }
});

// 2. Fechar ao clicar fora
$(document).on('click', function(e) {
  if (!$(e.target).closest('.div-botao-filtros').length) {

    $('.filter-dropdown.show').each(function() {
      var menu = $(this);

      // Remove a classe active do botão pai deste menu
      menu.closest('.div-botao-filtros').removeClass('active');

      menu.removeClass('show').addClass('hide');
      setTimeout(function() { menu.hide().removeClass('hide'); }, 150);
    });
  }
});


// Dropdown da navbar
$(function() {
  $('.navbar .dropdown-toggle').off('click');

  // Função para limpar os timers de todos os ancestrais
  function clearParentTimers($el) {
    $el.parents('.dropdown, .dropdown-submenu').each(function() {
      clearTimeout($(this).data('hideTimer'));
    });
  }

  // Lógica Unificada para Nível 1 e Submenus (Nível 2, 3...)
  $('.navbar').on('mouseenter', '.dropdown, .dropdown-submenu', function(e) {
    const $this = $(this);

    // 1. Limpa o timer do próprio elemento e de todos os pais
    clearTimeout($this.data('hideTimer'));
    clearParentTimers($this);

    // 2. Mostra o menu atual
    $this.addClass('show').children('.dropdown-menu').addClass('show');

    // 3. Fecha submenus "irmãos" que ficaram abertos
    $this.siblings().removeClass('show').find('.show').removeClass('show');

    e.stopPropagation();
  }).on('mouseleave', '.dropdown, .dropdown-submenu', function(e) {
    const $this = $(this);
    const related = e.relatedTarget;

    // ESTRATÉGIA CRUCIAL: Se o mouse se moveu para dentro de QUALQUER
    // descendente deste <li>, não inicia o timer de fechar.
    if (related && $this[0].contains(related)) {
      return;
    }

    // Inicia o timer para fechar
    const timer = setTimeout(function() {
      $this.removeClass('show').children('.dropdown-menu').removeClass('show');
      // Se fechar o pai, garante que limpa todos os filhos
      $this.find('.show').removeClass('show');
    }, 300);

    $this.data('hideTimer', timer);
  });

  // LÓGICA DE CLIQUE (Sua original, levemente otimizada)
  $('.navbar').on('mousedown', '.dropdown-menu a[data-value]', function(e) {
    e.stopPropagation();
    const $mainParent = $(this).closest('.nav-item.dropdown');
    const val = $(this).attr('data-value');
    const parent = val.includes('-') ? val.substring(0, val.lastIndexOf('-')) : val;

    if (typeof Shiny !== 'undefined') {
      Shiny.setInputValue('navmenu', val, {priority: 'event'});
    }

    const $side = $('.sidebar a[data-value="' + parent + '"]');
    if ($side.length) { $side[0].click(); }

    $('.navbar .nav-item.dropdown').removeClass('active-parent');
    $mainParent.addClass('active-parent');

    // Fecha tudo instantaneamente após o clique
    $('.navbar .show').removeClass('show');
  });

  // Mantendo o destaque dos botões circulares e abas simples
  $('.navbar').on('click', '.nav-item:not(.dropdown) > .nav-link', function() {
    $('.navbar .nav-item.dropdown').removeClass('active-parent');
  });

  $(document).on('click', '.circle-btn', function() {
    $('.navbar .nav-item.dropdown').removeClass('active-parent');
    $('.navbar .nav-item.dropdown').filter(function() {
      return $(this).text().trim().includes("Indicadores");
    }).addClass('active-parent');
  });
});


// Habilitando/desabilitando opções no input de comparações adicionais do gráfico de barras
$(document).on('shiny:connected', function() {
  // Função principal que gerencia o estado das caixinhas
  function gerenciarConflitoComparacao() {
    // 1. Pega o valor atual do Radio Button (Comparação Principal)
    // O seletor [name$='...'] pega o input que TERMINA com aquele nome (ignora o ns)
    var principal = $('input[name$=\"input_barras_comparacao_principal\"]:checked').val();

    // 2. Itera sobre todos os Checkboxes (Comparações Adicionais)
    $('input[name$=\"input_barras_comparacoes_adicionais\"]').each(function() {
      var box = $(this);

      // Se o valor do checkbox for igual ao selecionado no radio button
      if (box.val() == principal) {

        // Se estava marcado, desmarca e avisa o Shiny que mudou
        if (box.prop('checked')) {
          box.prop('checked', false);
          box.trigger('change'); // Essencial para atualizar o input$ no R
        }

        // Desabilita a interação (fica cinza)
        box.prop('disabled', true);
        // Adicional: deixa o label visualmente desabilitado também (opcional)
        box.closest('label').css('opacity', '0.5').css('cursor', 'not-allowed');

      } else {
        // Reabilita os outros casos
        box.prop('disabled', false);
        box.closest('label').css('opacity', '1').css('cursor', 'pointer');
      }
    });
  }

  // Executa a função toda vez que o Radio Button mudar
  $(document).on('change', 'input[name$=\"input_barras_comparacao_principal\"]', function() {
    gerenciarConflitoComparacao();
  });

  // Executa uma vez ao iniciar para garantir o estado inicial correto
  // O setTimeout garante que o DOM do Shiny carregou
  setTimeout(gerenciarConflitoComparacao, 100);
});

