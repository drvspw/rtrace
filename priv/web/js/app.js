window.addEventListener('load', () => {
  const el = $('#app')

  // Compile Handlebar Templates
  const errorTemplate = Handlebars.compile($('#error-template').html())
  const traceTemplate = Handlebars.compile($('#trace-template').html())
  const logsHeaderTemplate = Handlebars.compile($('#trace-logs-header-template').html())
  const logsTemplate = Handlebars.compile($('#trace-logs-template').html())

  const html = traceTemplate()
  el.html(html)

  // Router Declaration
  const router = new Router({
    mode: 'history',
    page404: path => {
      const html = errorTemplate({
        color: 'yellow',
        title: 'Error 404 - Page NOT Found!',
        message: `The path '/${path}' does not exist on this site`
      })
      el.html(html)
    }
  })

  // Instantiate api handler
  const api = axios.create({
    baseURL: '/api/v1',
    timeout: 5000
  })

  // Display Error Banner
  const showError = (title, message) => {
    const html = errorTemplate({ color: 'red', title, message })
    el.html(html)
  }

  // Trace API
  const API = {
    startTrace: async () => {
      try {
        const mod = $('#mod').val()
        const fun = $('#fun').val()
        const calls = parseInt($('#calls').val())

        const response = await api.post('/trace', { mod, fun, calls })
        const uuid = response.data

        // render the template and setup refresh handler
        let html = logsHeaderTemplate({ uuid, mod, fun, calls })
        $('#trace-logs-header').html(html)
        $('.refresh-trace').click(() => getTraceLogs(uuid))

        // get trace logs
        //getTraceLogs(uuid)
      } catch (error) {
        showError(
          'Unable to start trace',
          'Please check your module/function parameters and try again'
        )
      } finally {
        // remove loading indicator
        $('.loading').removeClass('loading')
      }
    },

    getTraceLogs: async uuid => {
      try {
        const response = await api.get(`/logs/${uuid}`)
        const logs = response.data
        let html = logsTemplate({ logs })
        $('#trace-logs').html(html)
      } catch (error) {
        showError('Unable to get trace logs', 'Please try again')
      } finally {
        // remove loading indicator
        $('.loading').removeClass('loading')
      }
    }
  }

  // Function to get trace logs
  const getTraceLogs = uuid => {
    $('#trace-calls-page').addClass('loading')
    API.getTraceLogs(uuid)
  }

  // Handle Convert Button Click Event
  const startTraceHandler = () => {
    if ($('.ui.form').form('is valid')) {
      // hide error message
      $('.ui.error.message').hide()

      // call start trace api
      $('#trace-calls-page').addClass('loading')

      API.startTrace()

      // Prevent page from submitting to server
      return false
    } else {
      $('.ui.error.message').show()
    }
    return true
  }

  router.add('/', () => {
    // first display the template
    let html = traceTemplate()
    el.html(html)
    $('.loading').removeClass('loading')
    try {
      // validate the ui form
      $('.ui.form').form({
        fields: {
          mod: 'empty',
          fun: 'empty',
          calls: 'integer[1..100]'
        }
      })
      // Specify Submit Handler
      $('.start-trace').click(startTraceHandler)
    } catch (error) {
      showError('Error', 'Unable to start trace')
    }
  })

  // navigate to current url
  router.navigateTo(window.location.pathname)

  // adding a click handler for menu items
  $('a.menu-item').on('click', event => {
    // Block browser page load
    event.preventDefault()

    // Highlight Active Menu on Click
    const target = $(event.target)
    $('.item').removeClass('active')
    target.addClass('active')

    // Navigate to clicked url
    const href = target.attr('href')
    const path = href.substr(href.lastIndexOf('/'))
    router.navigateTo(path)
  })
})
